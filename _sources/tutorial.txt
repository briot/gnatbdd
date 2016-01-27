.. highlight:: ada

********
Tutorial
********

This manual starts with a hands-on tutorial, that demonstrates various
aspect of gnatbdd.

The purpose of this tool is to act as a high-level testing framework, where
tests are described in such a way that they can be understood by the various
parties involved in the development of an application.

The calculator application
==========================

In theory, BDD (see :ref:`Behavior_Driven_Development`) development starts
with writing the tests, and then modify the code until the test passes.
However, in most cases you will be starting with an existing application, so
this tutorial will follow that typical case.

We will therefore assume we have implemented a very simple stack-based
RPN calculator (where operands are pushed on the stack, and then the operation
is applied to the top elements on the stack). Let's keep it very simple for now,
and assume we have the following :file:`src/calculator.ads` spec file::

    package Calculator is
        procedure Reset;
        --  Empty the stack

        procedure Enter (Value : String);
        --  Push a new operand on the stack (if Value is an integer) or
        --  compute an operation (only "+" is supported for now)
        --  The stack is limited to 5 entries.

        function Peek return Integer;
        --  Return the value at the top of the stack
    end Calculator;

It is not necessary to look at the body to test this application, so we will
omit it in this documentation. It is available in the gnatbdd source package
should you wish to reproduce and expand this example.

We also need to compile this application. For this, we will use a GNAT
project file, which also allows us to edit the application in GPS and use
gprbuild to build. The project file :file:`calc.gpr` should contains::

    project Calc is
       for Source_Dirs use ("src");
       for Object_Dir use "obj";
    end Calc;

First test: simple addition
===========================

..  highlight:: gherkin

Let's now write our first test, checking that we can do a simple addition.
Tests are written in text files with the :file:`.feature` extension (by
default). They will generally be found in a subdirectory, so let's create
the file :file:`features/first.feature`, with the following contents::

   Feature: simple operations
      A series of basic tests for simple operations

      Scenario: simple addition
         When I enter "1"
         And I enter "2"
         And I enter "+"
         Then I should read 3

Tests are called **Scenarios**, and they are grouped into **Features**. Each
Feature is supposed to deal with one specific aspect of the application, but
it might take several tests to fully test that feature. In the example above,
we have added a simple textual description of the feature, which is similar to
a high-level requirement for the feature.

We have then written, in English, a number of **steps** to execute for this
scenario.

Running the test
================

We need two steps to run the test.

The first step is to generate an executable, the **driver**, which includes
(part of) your application's source code and part of the library provided
with gnatbdd. We'll generate this executable with::

    > gnatbdd -Pcalc.gpr

The result of running this test are two new files in the object directory of your
project (in our case these are :file:`obj/driver.adb` and :file:`obj/driver.gpr`).

Let's then compile this driver::

    > gprbuild -Pobj/driver.gpr

This second step has generated the executable :file:`obj/driver`, which is in
charge of parsing each of our features files and execute the corresponding
scenarios.

The two steps above need to be performed only when your application's code
changes, or when the glue code (see below) changes. But you then use this
same driver to run (or re-run) any number of features files.

Let's do that now::

    > ./obj/driver --color=no

.. highlight:: gherkin

The output will by default use colors, which we cannot easily reproduce in this
manual. So we added the --color=no switch to explicitly disable colors, and so
that the output you get is exactly the one we show below. Feel free to experiment
without this switch to get colors (if your terminal supports them)::

    U
    Feature: simple operations
      A series of basic tests for simple operations
    
      Scenario: Simple addition   # first.feature#1:4
        When I enter "1"          # [UNDEFINED] first.feature:5
        And I enter "2"           # [UNDEFINED] first.feature:6
        And I enter "+"           # [UNDEFINED] first.feature:7
        Then I should read 3      # [UNDEFINED] first.feature:8
    
    
    1 features
    1 scenarios (1 undefined)
    4 steps (4 undefined)
    0m0.000s


The first line indicates that the driver has executed one scenario, where
it found undefined steps (i.e. it saw some text, but did not know how to
execute the corresponding code -- not so surprising, we have done nothing
so far). The rest of the output shows more details for the scenarios that
failed (by default, the driver does not show the scenarios that passed).

Writing the step definitions
============================

We now need to write a bit more code to match the English text that
describes the steps with the actual code to execute.

This is done through regular expressions associated with subprograms.
These subprograms will be searched for in either the sources of your
application (by using the project you passed in parameter to gnatbdd) or
in any number of additional directories. For now, we will do the latter
and create the step definitions outside of our standard project.

.. highlight:: ada

We thus create the following :file:`features/step_definitions/mysteps1.ads`::

    package MySteps1 is

        --  @when ^I enter "(.*)"$
        procedure When_I_Enter (Value : String);

        --  @then ^I should read (\d+)$
        procedure Then_I_Should_Read (Result : Integer);
    end MySteps1;

The most important thing to notice is that this is standard Ada, which will be
compiled through gprbuild as usual when we build the driver.

The second thing to notice are the two special comments which contain regular
expressions. They start with one of the keywords *@when*, *@given*, *@then*
(and a few others), which can be used interchangeably. These keywords are
followed by the actual regular expression. The full set of regular expressions
from :file:`GNAT.Regpat` is supported, although in most cases you will mostly
be using a lot of plain text including one or more parenthesis group. These
parenthesis groups will be automatically passed as parameters to the procedure
whose spec is just after this comment.

For instance, when gnatbdd sees a step that starts with "I enter" and then
some quoted text, it will call the subprogram `When_I_Enter` and pass it the
text as parameter.

The second example is slightly more interesting. The regular expression
expects to find a number after "I should read". This number is read as text,
but since our subprogram expects an `Integer`, gnatbdd will automatically
convert it to an integer before calling `Then_I_Should_Read`.

Let's write the body for these step definitions::

    with BDD.Asserts;  use BDD.Asserts;
    with Calculator;   use Calculator;
    package body MySteps1 is
    
       procedure When_I_Enter (Value : String) is
       begin
          Enter (Value);
       end When_I_Enter;
    
       procedure Then_I_Should_Read (Result : Integer) is
       begin
          Assert (Result, Peek);
       end Then_I_Should_Read;
    end My_Steps1;

This is very simple code, which needs to use both our application's code
(`Calculator`), and a part of the gnatbdd library which makes its easy to
compare integers, strings and various other types. When the assertion fails,
an error will be raised by the `Assert` procedure, and caught by the driver
to report an error to the user.

Remember we have put these step definitions in a directory that is not part
of our :file:`calc.gpr` project. We will need to pass this directory to
gnatbdd, which can either be done on the command line, or in general be done
directly from the project file so that we do not have to repeat them every
time we run gnatbdd. Let's modify :file:`calc.gpr` as such::

   project Calc is
      for Source_Dirs use ("src");
      for Object_Dir use "obj";
   
      package GnatBDD is
         for Switches use ("--steps=features/step_definitions");
      end GnatBDD;
   end Calc;

At this point, let's re-run gnatbdd and recompile the driver::

   > gnatbdd -Pcalc
   > gprbuild -Pobj/driver.gpr
   > ./obj/driver --color=no


.. highlight:: gherkin

And when we rerun our tests, we get the much more satisfying::

   .
   1 features
   1 scenarios (1 passed)
   4 steps (4 passed)
   0m0.000s


Adding multiple tests
=====================

Let's imagine we want to add more tests that will perform essentially the
same steps: enter the two operands on the stack, then enter the operation,
and check the result. We could simply copy-paste the scenario as we have
written it, but that would result in a very long features files.

Instead, we will use another feature, namely **Scenario Outlines**. Basically,
we will write a generic version of the test, and then a number of examples on
which we want to apply this generic. Here is the code we will now add to our
:file:`features/first.feature` file::

   Scenario Outline: testing operators
      When I enter "<first>"
      And I enter "<second>"
      And I enter "<operation>"
      Then I should read <result>
      Examples:
         | first  | second | operation | result |
         |  10    |  20    |   +       | 30     |
         |  20    |  10    |   -       | 10     |
         |  10    |  20    |   +       | 40     |

Each of the names between brackets will be substituted with the value in
the corresponding column of the table. Each row will result in one execution
of the outline. Since there are no new steps defined, and the application code
has not changed, we do not even need to rebuild the driver. We can directly
run the test, and get the following output::

    .F
    Feature: simple operations
      A series of basic tests for simple operations
    
      Scenario Outline: testing operators# first.feature#2:10
        When I enter "10"           # [OK] first.feature:11
        And I enter "20"            # [OK] first.feature:12
        And I enter "+"             # [OK] first.feature:13
        Then I should read 30       # [OK] first.feature:14
    
        When I enter "20"           # [OK] first.feature:11
        And I enter "10"            # [OK] first.feature:12
        And I enter "-"             # [FAILED] first.feature:13
          Exception name: PROGRAM_ERROR
          Message: Unknown operation: -
          Call stack traceback locations:
          0x10a80ccb3 ...
          at BDD.Asserts_Generic.From_Exception::bdd-asserts_generic.adb:139
        Then I should read 10       # [SKIPPED] first.feature:14
    
        When I enter "10"           # [OK] first.feature:11
        And I enter "20"            # [FAILED] first.feature:12
          Exception name: CONSTRAINT_ERROR
          Message: Calculator stack overflow
          Call stack traceback locations:
          0x10a80c919 ...
          at BDD.Asserts_Generic.From_Exception::bdd-asserts_generic.adb:139
        And I enter "+"             # [SKIPPED] first.feature:13
        Then I should read 40       # [SKIPPED] first.feature:14
    
    
    1 features
    2 scenarios (1 passed, 1 failed)
    16 steps (11 passed, 2 failed, 3 skipped)
    0m0.002s


The first line shows that 2 scenarios were executed, the first one
passed, the second one failed.

Hum, not quite clean, why do we have failures ?

* The first example line passed, since 10+20 is indeed equal to 30.

* The second line failed with an unexpected exception. That exception was
  properly caught by gnatbdd, which displays some details. If we look
  back at our :file:`src/calculator.ads` spec, we will see that in fact
  the only supported operation is "+", so "-" raises an exception.
  At this point, the stack of the calculator contains the result of the
  first scenario, the result of the first example, and the two operands
  we intended to use for "-".

* So when the third example runs, it pushes 10 on the stack (which now
  has five elements), and tries to push 20, but the stack overflows and
  we get another internal exception.

What we forgot to do here is to reset the calculator between each scenario
and each example. For this, we will introduce a new step definition for
"Given an empty calculator". We could add it to the first scenario, as
well as to the scenario outline. But then we'll have to remember to write
it for each scenario we might add to this features file. Instead, we will
define a **Background** block, which basically is a set of steps run before
each scenario and each example. Let's edit :file:`features/first.feature`
and add the following before the first scenario::

   Background:
      Given an empty calculator

We could generate, build and run our driver again, but we know this step is
undefined, so all scenarios will be marked as undefined.

.. highlight:: ada

It happens that we already have a subprogram that would be appropriate to
implement this step. This is `Calculator.Reset`. It is just missing the
regular expression, so we will edit the file and add it::

   package Calculator is

      -- @given ^an empty calculator$
      procedure Reset;

      ...
   end Calculator;


.. highlight:: gherkin

This time, the step is defined in the application sources, but as we
mentioned before these are already automatically parsed by gnatbdd, so
we can now generate, build and run::

   > gnatbdd -Pcalc
   > gprbuild -Pobj/driver.gpr
   > ./obj/driver --color=no

and now we get::

   ..F
   Feature: simple operations
     A series of basic tests for simple operations
   
     Scenario Outline: testing operators# first.feature#2:13
       When I enter "10"           # [OK] first.feature:14
       And I enter "20"            # [OK] first.feature:15
       And I enter "+"             # [OK] first.feature:16
       Then I should read 30       # [OK] first.feature:17
   
       When I enter "20"           # [OK] first.feature:14
       And I enter "10"            # [OK] first.feature:15
       And I enter "-"             # [FAILED] first.feature:16
         Exception name: PROGRAM_ERROR
         Message: Unknown operation: -
         Call stack traceback locations:
         0x10e254cb3 ...
         at BDD.Asserts_Generic.From_Exception::bdd-asserts_generic.adb:139
       Then I should read 10       # [SKIPPED] first.feature:17
   
       When I enter "10"           # [OK] first.feature:14
       And I enter "20"            # [OK] first.feature:15
       And I enter "+"             # [OK] first.feature:16
       Then I should read 40       # [FAILED] first.feature:17
          40 /=  30
         at MySteps1.Then_I_Should_Read::mysteps1.adb:14
   
   
   1 features
   2 scenarios (1 passed, 1 failed)
   17 steps (14 passed, 2 failed, 1 skipped)
   0m0.002s


The last exception is gone, and we now get an error (yes, 10+20 is not 40).
Notice how the call to `Assert` lists both the expected and actual values,
which is often enough to understand the error without entering the debugger.

We can fix the features file to expect 30 instead of 40.

At the same time, we implement support for "-" in our calculator, since this
is now needed to pass our tests. This is the traditional BDD approach: write
the test first, then write the code.


Using tables
============

We add an issue previously with the contents of the stack. Let's add a new
test that will check this content.

The idea is to enter a number of values on the stack (we already have a
step definition for this), and then compare the stack with an expected
output. To describe this expected output, we will make use of another
aspect of the features language, the **tables**. We have already seen
an instance of such a table when we defined a scenario outline before,
although that was in a slightly different context.

.. highlight:: gherkin

Let's add the test to :file:`features/first.feature`::

   Scenario: Checking stack contents
      When I enter "10"
      And  I enter "20"
      And  I enter "30"
      And  I enter "40"
      Then the stack should contain
         | value |
         | 10    |
         | 20    |
         | 31    |
         | 40    |


.. highlight:: ada

And, as usual, we need to provide the step definition. Let's add it
to :file:`features/step_definitions/mysteps.ads`, although it could of course
be in any other source file::

   with BDD.Tables;    use BDD.Tables;
   package MySteps1 is

      --  @then ^the stack should contain$
      procedure Then_The_Stack_Should_Contain (Expected : BDD.Tables.Table);
   
   end MySteps1;

There are a few differences with our previous step definitions. Since the table
is given on a different line, it should not be part of the regular expression.
But we still want to indicate in the parameters of the subprogram that a table
is expected. That should be the last parameter, and its type should be exactly
`BDD.Tables.Table` (this is a textual comparison, no cross-reference from the
compiler). With this, gnatbdd will automatically recognize that a table is part
of the step and will be passed to the subprogram.

Let's look at the implementation::

   package body MySteps1 is

      procedure Then_The_Stack_Should_Contain (Expected : Table) is
         Actual : Table := Create;
         Stack  : constant Integer_Array := Peek_Stack;
      begin
         for S in Stack'Range loop
            Actual.Put (Column => 1, Row => S, Value => Stack (S)'Img);
         end loop;

         Assert (Expected => Expected, Actual => Actual);
      end Then_The_Stack_Should_Contain;

   end MySteps1;

Here we build another table from the actual contents of the stack, and do a
diff between the two tables. We'll see when we run the test that gnatbdd is
able to nicely display diffs in table, using colors when possible, so that it
is easy to see at a glance where any error is.

In the best BDD tradition, we have now implemented a test before we even had
the code for it in our calculator, so now we need to improve our calculator
until the test passes. Here goes the change in :file:`src/calculator.ads`,
with a corresponding body (not shown here)::

   package Calculator is
       ...

       type Integer_Array is array (Natural range <>) of Integer;
       function Peek_Stack return Integer_Array;

   end Calculator;

With these various additions, we can now run our test as usual::

   > gnatbdd -Pcalc
   > gprbuild -Pobj/driver.gpr
   > ./obj/driver --color=no


.. highlight:: gherkin

::

   ...F
   Feature: A first feature
     This is not very useful, we are just testing that we can execute basic
     calculator tests.
   
     Scenario: Checking stack contents   # first.feature#3:25
       When I enter "10"                 # [OK] first.feature:26
       And  I enter "20"                 # [OK] first.feature:27
       And  I enter "30"                 # [OK] first.feature:28
       And  I enter "40"                 # [OK] first.feature:29
       Then the stack should contain     # [FAILED] first.feature:30
         | 10       |
         | 20       |
         | 30 /= 31 |
         | 40       |
   
   
   1 features
   3 scenarios (2 passed, 1 failed)
   22 steps (21 passed, 1 failed)
   0m0.003s


The test is obviously wrong, so let's fix the test, rerun, and all is clean !

This concludes this small tutorial. The rest of this document will go into
further details for each of the steps we have seen here, as well as quite a few
additional capabilities in gnatbdd.





