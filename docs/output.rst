**********************
Controlling the output
**********************

The driver provides a rich set of options for controlling the output.

Colors
======

.. index:: switches; --color

Color output is enabled by default when the terminal supports it. It is used to
highlight passing, failing and skipped tests. The driver attempts to detect
automatically whether advanced features like colors or moving the cursor are
supported. However, you can also force a specific setting by using one of:

* :option:`--color=yes`

  Forces the use of colors in the output. This is in particular useful when the
  output is redirected to a file which you intend to display in a console later
  on. The driver always uses ANSI escape sequences when redirecting to a file,
  so on Windows you should install `ANSICON <https://github.com/adoxa/ansicon/>`_.
  This is not necessary when you are directly displaying the output in the
  console though.

* :option:`--color=no`

  Disable the use of colors. The driver will output strings such as "[OK]",
  "[FAILED]" or "[SKIPPED]" instead.

* :option:`--color=auto`  (the default)


Quiet and verbose output
========================

.. index:: switches; --output

Depending on your use of the tests (and for instance whether you are using
GNATbdd interactively or as part of an automatic testsuite), the amount of
output needs to be controlled.

Typically, a user developing the application needs to get as much information
as possible directly in the console, so that hopefully it is easy to undertand
why a test is failing. In fact, some users might want to see the passing tests
as well, just to make sure the testsuite is moving forward.

However, during an automatic test, all the output can be dropped, and we are
only interested in getting a final count of passing and failing tests.

All these behavior can be controlled with the following command line
switches:

* :option:`--output=quiet`

  The driver only displays the final count for each categories of
  tests (passed, failed and skipped).
  The output therefore looks like::

      5 scenarios (1 skipped, 2 failed, 2 passed)
      0m1.002s

* :option:`--output=dots`   (the default)

  The driver displays a "." for each test that passes, a "F" for each
  test that fails (and will then display a full backtrace for those)
  and a "S" for skipped tests.
 
  When all features have run, the driver displays details for all tests
  that did not pass.

  Finally, it displays a summary for the final count for each category.

  The output therefore looks like the following (although colors will be
  used depending on the :option:`--color` switch)::

      .F...

      Feature: controlling the output
        Scenario: quiet and verbose output   # features/output.feature:3
          Given 2 failing scenarios          # features/output.feature:4
            and 2 passing scenarios          # features/output.feature:5
          Then the output should look like "..."  # [FAILED]

      5 scenarios (1 failed, 4 passed)
      0m1.002s

* :option:`--output=hide_passed`

  In this mode, the driver does not display anything when a scenario passes,
  but displays the same backtrace as above for failed or skipped. One
  advantage is that the output of the full traces is done after each
  scenario has been run, not as a summary after all of them have run.

* :option:`--output=full`

  In this mode, the driver displays output of all scenarios, whether they
  pass or fail. The output looks like the above, but instead of displaying
  a ".", the driver outputs the full lists of steps for that scenario.

  In this mode, the driver outputs each step before it is being run, thus this
  allows you to monitor which step is taking long to execute.

Redirecting the output
======================

.. index:: switches; -o

By default, the driver will print its output on the screen. It is possible
though to print to a file by using the :option:`-o FILE` switch.

.. index:: HTML output

If the file ends up with the :file:`.html` extension, the driver will
generate HTML output rather than textual output. This will display nicer
in a web browser.

Log files
=========

.. index:: switches; --log

In addition, the driver can still generate log files which contain output
similar to the above, as well as any output done by your application
on stdout or stderr.

One log file is generated per scenario. Its name is that of the features
file and the line number at which the scenario starts. The switch
:option:`--log=DIR` can be used to control the directory in which the
log files are created. The default is to create them in a :file:`logs/`
subdirectory of the directory where GNATbdd is run.

.. note::
   Should this be in the root project's object_dir, when GNATbdd is run
   with a project in argument ?

Exit status
===========

On exit, the driver will set its status to the number of failed scenarios.
As a result, it is easy to check whether there are any unexpected failures.
