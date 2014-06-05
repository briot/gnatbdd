Customizing the BDD library
===========================

When you write your own step definitions, you will likely
use part of the library provided by GNATBDD. This library
provides a convenient API for comparing values and reporting
errors, or for manipulating tables for instance.

This library is also used by the generated driver itself to
control its output.

The library can be extended at several levels.

Customizing the output format
-----------------------------

By default, GNATBDD supports a number of output formats
(text and HTML). You could add new formats (like XML,
JSON or a custom format for instance) by extending the
type `BDD.Media.Media_Writer` and overriding the various
primitives.
