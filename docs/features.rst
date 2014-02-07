..  highlight:: gherkin

********
Features
********

An application's requirements are organized into features. Such a feature has a
name (for instance one feature we would use for GNATbdd itself would be "Find
features to run"), as well as a textual description. This description is only
meant for human readers, and is not used by the automatic tool.

The features are then further divided into one or more scenarios, which are
sometimes also called "tests". These will be discussed in the next section.

Finding features
================

Let's first examine how GNATbdd finds all the features for your application.

.. index:: switches; --ext

Typically, the features are written in text files with a :file:`.feature`
extension. Several editors will automatically provide syntax highlighting with
such an extension. However, you can choose your own extension by using the
:option:`--ext .EXT` switch when you run GNATbdd.

.. index:: switches; --features

By default, GNATbdd will find all the files with a matching extension in
a subdirectory names :file:`features`, within the directory where GNATbdd
is run. You can however specify one or more other directories to check,
by using the :option:`--features DIR` switch one or more times.

GNATbdd will always search recursively in those directories. This lets you
organize the files into subdirectories when you have several of them, and
thus perhaps make it easier to refer to them.

GNATbdd will always run the features file in alphabetical order on the full
path of the feature file). This provides a more consistent output when
GNATbdd is run multiple times.

Syntax of the features file
===========================

The syntax of the features file is based on the one from Cucumber, which is
a BDD standard tool in the Ruby world. The grammar and syntax in those files
is voluntarily information, since they are meant to be editable and readable
by people other than software developers.

As a result, this documentation is mostly based on examples. Let's start
with the first general layout for a file::

  Feature: name of feature
     high-level description
     of what the feature does (plain English, not used by the tools)

     Scenario: name of scenario
        Given some precondition
          And some other precondition
        When I do some action
         And I do some other action
        Then I should see a specific result
         And I see something else
         But I do not see something else

There could be several **scenarios** within the **feature**. In fact, there
could also be several features in a given file, but this is not recommended in
general (this is an extension of the format used by cucumber).

The scenario is split into several **steps**, each of which start on a separate
line. The steps are introduced by one of severaal keywords, all shown in bold
in the example above. You can use any of the keywords for any of the steps, but
in general they have the following semantic:

* **Given** puts the system in a known state, before the user starts
  interacting with it. Avoid talking about user interaction in givens.  These
  are similar to preconditions in programming languages.

  For instance, a given would setup a database, log in a user, and so on.

* **When** describes the actions performed by the user, like clicking on
  elements, providing input, and so on.

* **Then** observes the outcome of the actions. These observations should be
  related to the business benefit that was described in the feature.  The
  observation should be on some kind of output that is something that comes out
  of the system (report, user interface, message,...) and preferrably not
  something deeply buried in the system (use unit tests for those instead).

The example above indents each level of the description. This is not
strictly mandatory, but helps make the file more readable.

The keywords are case-sensitive, as is done in other BDD tools.

Comments
--------

A feature file can contain comments anywhere. These are lines whose first
non-blank character is '#'. The comment extends to the end of the line.

Tagging
-------

Features and scenarios can be tagged with one or more tags. These tags are
specific to your application and usage of GNATbdd. Primarily, they can be used
to run subsets of the whole set of scenarios. Here is an example::

   @gui @editor @req-1-1
   Feature: Opening an editor restores the previous location

      @startup
      Scenario: Restore open editors and their location on startup
         Given a previous run that was editing foo.adb at line 5
         When I start the application
         Then I should see a window foo.adb at line 5

The tags of the feature automatically apply to its Scenarios

.. index:: switches; --tags

When you run GNATbdd, you can use the switch :option:`--tags` to control which
scenarios should be run. For instance, all scenarios related to '@gui', or all
scenarios not related to '@startup'. You can of course select subsets of
scenarios based on the file names, but tags provide a file-system-agnostic
selection mechanism.


Other usage of tags could be to identify *slow tests* (with @slow) so that
their timeout is increased.

A tag can also be used to link a scenario to a *high-level requirement* in your
application

Tags can also be used to identify *expected failures* (for instance @xfail), or
*work in progress* (for instance @wip).


Step configuration
------------------

Steps describe the actual actions to perform on the software, its input or its
output. In the examples above, we have seen various sentences used to describe
those actions. However, if we have to write a different sentence for every
little variation, this will end up being very difficult to maintain indeed.

So instead, the steps can be configured so that they apply to a wide variety of
scenario. For instance, going back the example on the editors above, there is
nothing specific in the test about the name *foo.adb* or the line *5*. We might
want to rerun a similar step on file *bar.adb* at line *10*.  As we will see
when we discuss the definition of steps, this is of course doable.

But staying closer to the topic of the syntax, there are two other ways that
the steps can be configured, namely **multi-line strings** and **tables**.

* multi-line strings are convenient when the text to substitute contains
  several lines. They can only be used as the last part of the step, as in
  the following example::

      Feature: Entering multiple lines of text in the editor
         Scenario: Pressing the return key on the keyboaard
            Given a blank editor
            When I press the keys <a>, <enter>, <b>
            Then the editor should contain
              """
              a
              b
              """

  A multi-line string starts on a line of its own just after the step itself.
  It starts with three double quotes (this is a notation that is familiar to
  all Python developers), and ends on a similar line that contains double-quotes.
  The double-quotes must appear on a line of their own.

  We recommend indenting the quotes and their contains relatively to the step
  itself to improve readability.

  The lines between the quotes form the text that is used for the step itself.
  Those lines are unindented by an amount equal to the indentation of the first
  quotes line (so in the example above there will in fact be no whitespace
  before 'a' and 'b' when we compare them to the actual output). If a line does
  not start with enough white spaces, GNATbdd simply removes all leading white
  spaces, but preserves the first non-white character.

* tables are another great way to provide input. They organize their data into
  columns, which are interpreted by the step as it sees fit. Here an example::

     Feature: Logging in on a website
       Scenario: Logging with valid user account
          Given the following users exist
            | Name   | Email            | Phone |
            | John   | john@example.com | 1234  |
            | Jack   | jack@example.com | 5678  |
          When I log in as "Jack"
          Then I should see the home page


Background scenario
-------------------

The givens in the last scenario above (providing the name of multiple users for
a web site) would need to be duplicated if we wanted another scenario that tests
logging in with an invalid user. Obviously, duplication is just as bad in tests
as it is in the code itself.

Instead, you can defined a background for the feature. It defines steps to be
performed before running each of the step in the scenario. For instance, the
feature above would be better written as::

     Feature: Logging in on a website
       Background:
          Given the following users exist
            | Name   | Email            | Phone |
            | John   | john@example.com | 1234  |
            | Jack   | jack@example.com | 5678  |

       Scenario: Logging with valid user account
          When I log in as "Jack"
          Then I should see the home page

       Scenario: Logging with invalid user account
          When I log in as "Henry"
          Then I should see the login page


The background must be defined before any scenario.
    

Scenario outlines
-----------------

We mentioned before that parts of the steps can be configured. For instance, we
could have a feature with the following two scenarios::

     Feature: Testing addition in a calculator
       Scenario: adding simple numbers
          When I enter 5
           And I add 12
          Then I should get 17

       Scenario: adding larger numbers
          When I enter 105
           And I add 1012
          Then I should get 1117

The two scenarios are very similar, this is another case of duplication that
would best be avoided.

The feature file provides the notion of a **Scenario Outline**, which provides
text substitution to create multiple scenarios. Here is the example above
rewritten by taking advantage of this feature::

     Feature: Testing addition in a calculator
       Scenario Outline: adding simple numbers
          When I enter <num1> 
           And I add <num2>
          Then I should get <result>

       Examples:
          | num1  | num2  | result |
          | 5     | 12    | 17     |
          | 105   | 1012  | 1117   |

The **Examples** provide the values to substitute in the steps above. There
will be one scenario executed for each line in the examples.

For compatibility with other tools, the keyword **Examples:** can be replaced
with **Scenarios:**.
