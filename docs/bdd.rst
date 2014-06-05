.. _Behavior_Driven_Development:

***************************
Behavior Driven Development
***************************

Test Driven Developent (TDD) describes the cycle of writing a test first, and
application code afterwards. These tests could be unit tests, up to end-to-end
("black box") tests. TDD doesn't make any statements about what should be
tested, or how tests should be organized and named.

An extension, Behavior Driven Development (BDD) has been suggested. Based on
various proposed templates, the user describes, in natural language, what an
application feature is intended to do, and various scenarios which the final
user might be applying.

Basically, these natural language files answer three questions for each
scenario:

* **In order** to get some benefit
* **As** the user you are developing for
* **I want** what this feature does

This description is always followed by a list of scenarios containing **Given**
steps (what has happened before), **When** steps (what actions the user
performs), and **Then** steps (the desired outcome for the user).

Testing an application
======================

BDD is not intended to replace unit tests. Instead, it is meant as a complement
to them. Unit tests (as written for instance with :program:`Aunit`, or
partially automatically generated with :program:`gnattest`) are white-box
testing, that apply to specific pieces of code. These are developer-level
tests, which are in general impossible to understand for potential users of the
application.

BDD thus proposes to write the feature descriptions and their scenarios as
plain English tests in conjunction with the various stake holders. These
description are then processed automatically later on to test that the
application performs as expected.

These tests are also different from the black-box testing that is often used,
when the application is run within a specific environment, and the output is
compared to an expected baseline. The intention in BDD is to be explicit in
*what* we are comparing in that output. Very often, the test is only intend
to check a very small part of the full output, and any variation in the rest
of the output is irrevant to the particular test we are running.

Similar software
================

.. note::
   This section is for AdaCore internal use only

There exist other similar software already.

* :program:`cucumber` is a de-facto reference in the world of test-driven
  development.
  It is developed in Ruby, and provides quite a lot of facilities to test
  web servers (preferably written in Ruby) and web clients. Adding new
  step definitions needs to be done in Ruby, although there exists a number
  of bridges to C++ and Java for instance.
  Using it requires a Ruby setup on the developer's machine, and there is
  no built-in support for writting tests in Ada

* :program:`Xreq` is a tool implemented by Sogilis, which has been used for
  the testsuite of CRM, GNAT Tracker and bugtool for a while. This tool is
  very close to :program:`cucumber`, and uses a similar syntax for the test
  description files.

  It has several drawbacks, though: its implementation is 'experimental',
  and lacks documentation. Its output is sometimes confusing, and we made
  several iterations to make it usable, although it could be improved.
  This tool is not distributed outside of Sogilis and AdaCore, so it has not
  been extensively tested either.
  No support for running tests in parallel, no asserts library to ease
  writting tests, no support for cross development. No predefined steps or
  regular expressions.

  It also lacks predefined regexps, step definitions, or even a full fledge
  assertions library that would make it easier to write new steps.

* :program:`Fitnesse` uses tests written in a Wiki, and directly highlights
  the web page to show passing and failing tests.
  One of its nice aspects is that tests can be really mixed in with plain
  English text for the requirements or design document.

