=====
Ideas
=====

.. note::
   This section is specific to AdaCore


Here are the various ideas we currently have.


Unit tests
==========

This BDD framework is usable in a context of unit testing.  However, creating
one new step per function we want to test is heavy. One approach might be to
have a special tag which indicates the name of the function that is being
tested::

    @subprogram: subp_name
    Scenario Outline: Testing a subprogram
       When I pass the parameters <param1>
       Then I should get <result>

       Examples:
           | param1 | result |
           |   2    |    3   |
           |   3    |    4   |


This requires help from the code generation step so that we automatically
generate steps based on the @subprogram tags


Input format
============

The :file:`.feature` file format is a kind of standard for BDD, and is
supported by multiple tools out there out of the box (Sphinx, vi,...).

However, in our context, it might make sense to provide a second parser that
would embed the scenarios in a Rest document. This would allow us to easily
generate documentation from the tests.


Cross testing
=============

Testing embedded softward presents several challenges.

The most important goal for Gnatbdd is that we do not force the user to
embed any addition code (from the BDD library) into his application. This
would restrict what we can do in the BDD library.

So instead the idea is that all the parsing of the feature files, and the
matching with the regexps be done on the host. The implementer of the steps
is then responsible for running code on his target.

Of course, we might want to provide additional support libraries to make
the coding of steps easier. For instance:

* provide code to help spawn an application on a board or in QEmu.
* provide code for communicating with a running application, for instance
  via sockets (when available, e.g. VxWorks), via gdb (available on most
  targets), via QEmu,..

This library could be reused in other contexts, like AUnit for instance
for the cases where we do not want to run the AUnit code on the target
for instance.

Black box testing in this context means spawning the application, and
comparing its output, which can be done more or less generically.

White box testing can be done through gdb, or via having specific code in
the application. So the user would for instance have the following setup::

       Host                                   Target
       ====                                   ======
       gnatbdd
          |
       match step with regexp
          |
       executes Ada code for the step
          |
       User Ada code communicates with
       target, passing a step number and
       arguments
                \------------------------------ Run a test function that
                                           has a big case statement testing
                                           the step number.
                  -----------------------------
                 /
        Compare the output, and raise assert_error
        as needed

We could also for instance have a special tag @remote_call (suggested by
Thomas)

Jerome suggested another approach: at compile time, we generate code for the
scenario (basically all the calls to the step subprograms), and this is the
code that gets sent to the target and executed remotely by the GNATbdd
framework.

GPS testing
===========

GPS is a complex application to test, because most steps (like clicking a
button) require that the execution of the next step waits for a specific delay
or event.

These asynchronous steps could be handled in several ways:

* Run GPS as a separate process (spawned by a first step), then all other
  steps communicate with it via sockets (GPS server), and wait for a response
  from GPS indicating we can continue. This requires two way communication,
  and might not be very efficient.

* We could also generate a set of python code to be executed by the test and
  send this all at once to GPS, and then parse the result. Should be more
  efficient, but needs a small protocol so that GNATbdd can report which
  step failed.

* Another approach is to run the scenario in a thread, which automatically
  suspends itself when the step is asynchronous and waits for the main
  GNATbdd thread to unblock it. GNATbdd would do this under some conditions,
  and the user would provide the necessary support subprogram. For instance,
  in the case of GPS we would setup a timeout or idle callback.
