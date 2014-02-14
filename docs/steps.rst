*****************
Steps definitions
*****************


Compiling steps
===============

Now that you have described in the features file what the test should be doing
and what the expected result should be.

This is performed via the various steps defined in the scenario.  We now need
to associate those steps (English sentences) into actual code.

This is done in one or more Ada files which are compiled and linked into the
GNATbdd framework to generate an executable.  This executable is then run and
dynamically parses all the :file:`*.feature` file as we described in the
previous step::

      Ada BDD library      Ada step definitions
              \                  /
               \                /
                \              /
                   GNATbdd
                      | generate glue code
                      | <compile and link>
                      |
                      \                feature files
                       \                   /
                        \                 /
                         \               /
                        test driver (exec)


.. index:: switches; --steps

The step definitions are found automatically by parsing the Ada package specs
found in the directory :file:`features/step_definitions` (by default, although
you can use the :option:`--steps=DIR` switch to point to another directory. All
the Ada files found in those directories (recursively) will be parsed for
subprograms that have the Step_Regexp aspect or pragma (see below).

It is possible to mix the step definitions with your standard application code,
but this is not a recommended approach and it is better to separate the two.

When you launch GNATbdd, it will search for all the Ada files in those
directories, generate one Ada file, and will then link your Ada files, the
generated file, and the relevant parts of the GNATbdd library. This will
generate a single executable that is automatically spawned and then in
charge of processing all your :file:`.feature` files.

.. note::
  This compilation is done by generating an extending project file, so users
  will also have to provide a project file that points to all their sources.

As a result, the above compilation step will only take time whenever you
add or change step definitions, but not when you only add :file:`.feature`
files that reuse existing features.

Add files for step definitions
==============================

The steps themselves can basically perform any action you want, and they
define whether you are doing black box or white box testing (see below).

.. highlight:: ada

The Ada packages should contain code similar to the following::

   package My_Steps is

      --  @given ^a user named (.*)$
      procedure Set_User (Name : String);

      --  @then ^I should get (\d+) as a result$
      procedure Check_Result (Expected : Integer);
   end My_Steps;

The example above shows two steps defined with special comments.
The comment must occur just before the subprogram to which it applies.

.. note::
    Should support custom aspects ?
    With comments, how do we handle cases where the regexp is too long
    to fix on a line, except for using pragma Style_Checks(Off).

The comments should start with one of '@given', '@then' or '@when'.
There is no semantic difference, they only act as a way to help
introduce the regexp.

The regular expressions are matched with the step as found in the
:file:`*.feature` file. The parenthesis groups found in the regexp will be
passed as parameters to the procedure. By default, all parameters are passed as
strings. If you use another scalar type for the parameter, GNATbdd will use a
`Type'Value (...)` before passing the parameter, and raise an error if
the type is incorrect.

It is recommended that regular expressions always be surrounded with '^' and
'$', to indicate they should match the whole step definition, and not just part
of it.

.. index:: switches; --duplicates

The switch :option:`--duplicates` can be used to systematically check all
regular expressions for each step, and warn when there are multiple matching
regexps.

.. note::
   alternatively, we could output the name/location of the subprogram that
   handled the step, to ensure the right one is executed.

Assert library
==============

The intent is that the steps should raise an exception `Assert_Failure`
when the step fails. GNATbdd provides the package :file:`BDD.Asserts` to help
perform the tests and raise the exception when they fail. This package will
also make sure a proper error message is logged, showing the expecting and
actual outputs.

For instance, the implementation for one of the steps above could be::

   with BDD.Asserts;   use BDD.Asserts;

   package body My_Steps is
      procedure Check_Result (Expected : Integer) is
         Actual : constant Integer := Get_Current_Result;
      begin
         Assert (Expected, Actual, "Incorrect result");
      end Check_Result;
   end My_Steps;

When this test fails, GNATbdd will display extra information, as in::

   Then I should get 5 as result     # [FAILED]
      Incorrect result: 5 /= 4 at my_steps.adb:7

Many more variants of `Assert` exist, which are able to compare a lot of
the usual Ada types, as well as more advanced types like lists of strings, or
the tables that are used in the feature files to provide data to steps.

.. note::
   when comparing steps, the output should highlight the parts of the string
   that are different, to help spot the difference, We also need to do
   something special for trailing spaces.

Predefined Regular Expressions
==============================

To simplify the writting of your steps, GNATbdd provides a number of predefined
regular expressions that can be used in your own regular expressions. These
expressions have a name, that can be used in your regexps by using a leading
colon, as in::

    procedure My_Step (Expected : Integer)
       with Step_Regexp => "^I should get :natural results";


Here is the full list of predefined regular expressions:

+---------+----------------------------+-------------------+
| name    | examples                   | Ada type          |
+=========+============================+===================+
| integer | -1; 0; 234                 | Integer           |
+---------+----------------------------+-------------------+
| float   | -1.0E+10; 002E-10          | Float             |
+---------+----------------------------+-------------------+
| natural | 2; 56                      | Natural           |
+---------+----------------------------+-------------------+
| date    | Feb 04, 2014; 2014-02-04   | Ada.Calendar.Time |
+---------+----------------------------+-------------------+


Predefined steps
================

GNATbdd itself includes some predefined steps, which you can immediately use
in your :file:`.feature` file.

Here is the full list of predefined steps:

* `When I run '.*'`

  This step can be used to spawn an executable (possibly with its arguments) on
  the local machine.

* `Then (stdout|stderr) should be .*`

  Compare the contents of standard output or standard error with an expected
  output. In general, the last argument would be specified as a multi-string
  argument in your :file:`.feature` file.


.. note::
   When an application is using AWS, we could have predefined steps to connect
   to a web server and compare its output (json, html,...)


Missing step definitions
========================

When the :file:`*.feature` files contain steps that have no corresponding
definition, they are are highlighted in a special color, and GNATbdd will
display possible definitions for the corresponding subprograms, which you can
copy and paste into your Ada file directly. This helps getting started.


Step timeout
============

.. note::
   This will likely require each scenario to be run in its own task. There
   will be only one such task, so it doesn't really add constraints on the
   user code or step definitions, but it makes debugging slightly more
   difficult.

.. index:: switches; --timeout

You can use the :option:`--timeout` switch to specify a maximum time that
steps can take to execute. A test that times out will automatically fail
with an appropriate error message.


Writing steps in python
=======================

.. highlight:: python

Steps can also be defined in python, by creating python files with contents
similar to::

    @step_regexp("^I should get :num as a result")
    def check_result(expected):
        current = get_current_result()
        if expected != current:
            raise AssertionError(
               "Invalid result %d != %d" % (expected, current))

As usual, any python file found in the :file:`features/step_definitions`
directory or the one set through :option:`--steps` will be analyzed,
and those that use the `@step_regexp` decorator.
   

Asynchronous tests
==================

.. note::
   Note sure how to implement this yet.

In some cases, it is necessary to stop executing steps to give some time for
the application to complete its handling, and then come back to the execution
of the test. In particular, this is often necessary when testing graphical
user interfaces and other event-based applications.


Running tests in parallel
=========================

There are multiple modes to run features in parallel. The parallelism is always
between :file:`.feature` file, never between the Scenario of a given file.

When your application is task safe, you can run multiple features in parallel
by running each in its own task (up to a maximum number of tasks of course).

In other cases, GNATbdd can automatically spawn several instances of the test
driver, each running a single feature in parallel of the others. This is
slightly less efficient, but does not impose task-safety constraints on
your application.


White box vs Black box testing
==============================

Testing can be done in various ways, and this section tries to provide a
few leads on how to organize and perform your tests.

Black box testing
-----------------

In this mode, the application is spawned with specific arguments, and all
interaction with it (input or output) is done only as a user would.  It is not
possible to examine the value of specific variables, unless they have a direct
impact on what can be seen from the outside.

The main advantage is that the application is tested exactly as the user would
use it. This mode is compatible with most applications, like command-line
application, graphical user interfaces, web servers or embedded applications.

When testing embedded applications, GNATbdd will run on the host, and the
application will be spawned on the target. Communication between the two is the
responsibility of the step definition, and could take the form of examining the
standard output or communicating via sockets for instance.

No real restriction apply to the way the step definition is written, since it
is running on the host, not in the more limited environment of the target.

White box testing
-----------------

In this mode, the step definitions can access all the public parts of your
application's code (or at least the public part of it). As a result, it is
possible to inspect in details the actual start of your application, and
perhaps catch errors earlier in the code.

One of the inconvenients in this mode is that the steps themselves end up
dragging in a lot of the application's code, which makes the link time for
the driver longer.

More importantly, this mode might not be compatible with embedded development,
since GNATbdd runs on the host.

.. note::
   Can we run the steps directly on the target in this case, while limiting
   what features of the code we use like controlled types, memory
   allocation,...

White box testing can itself be done in one of two ways: either be linking
the application's code within the GNATbdd driver (because the code for the
steps would `with` the application's own packages), or by spawning an
executable and communicating with it via various means (stdin/stdout,
sockets, pipes,...)
