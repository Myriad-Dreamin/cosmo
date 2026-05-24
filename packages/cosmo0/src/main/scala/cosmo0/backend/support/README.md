# support

This directory contains support-library metadata shared by backend output and
package integration.

`Library.scala` defines support-library ids, platform and artifact kinds,
available artifacts, link items, link plans, and validation diagnostics.

The C++ backend uses these definitions to turn backend requirements into link
arguments. Package and fixture tests use them to validate missing or incompatible
support artifacts.
