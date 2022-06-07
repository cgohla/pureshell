# PureShell #

Compile [PureScript](https://www.purescript.org) to shell scripts.

The PureScript compiler fortunately makes it easy to write alternative
backends, of which [there are
several](https://github.com/purescript/documentation/blob/master/ecosystem/Alternate-backends.md).

# Code Overview

Most modules are under the `Language.PureShell` hierarchy. We have
several intermediate languages, these are in order:

- `CoreFn`: This is our input language and is defined by the
  [PureScript
  compiler](https://github.com/purescript/purescript/tree/master/src/Language/PureScript/CoreFn). It
  is a form of untyped lambda calculus.
- `Combinatory`: Mostly mirrors CoreFn, but in abstractions all free
  variables have to be bound simultaneously.
- `Procedural`
- `Bash`: The output language

For each of these we have at least the following three modules, namely

- `IR`: AST type definitions
- `CodeGen`: helpers
- `Lower`: high level functions for mapping to the next lower stage

(This structure is heavily inspired by [_How to lower an
IR?_](https://luctielen.com/posts/how-to-lower-an-ir/).)
