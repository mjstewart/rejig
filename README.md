# Rejig

***organise (something) differently; rearrange.***

A *"module header"* (import / export declarations) formatting tool for `haskell` and [daml](https://github.com/digital-asset/daml).

This tool provides an opinionated strategy for grouping and deep sorting import/export declarations by *structural components* instead of by simple alphabetical module names.

`rejig` isn't invasive and doesn't attempt to be a code formatter. It simply formats the module header section and leaves the rest of the source code unchanged.

# Installation

`rejig` is a cli tool and requires the binary to be on the `$PATH`.

### 1. Download binary
https://github.com/mjstewart/rejig/releases

### 2. Add to path

Use your preferred method of adding `rejig` to your `$PATH`

Here's an example of my linux setup.

```
-- I keep user apps in /opt so I move the download there.
mv ~/Downloads/rejig /opt

-- Ensure its executable
chmod +x /opt/rejig

-- symlink so its available somewhere on $PATH
ln -s -f /opt/rejig /usr/bin/rejig
```

I don't use windows so I don't know if this will work, the binary *"should"* work if invokved using `rejig.exe`.

# Usage

```
Usage: rejig
  (--file FILEPATH | --stdin DESCRIPTION)
  (--haskell | --daml)
  [--prefixes ARG1 ARG2 ...]
  [--titles]
  [--border-top]
  [--border-bottom]
```

## typical examples

via file
```
rejig --file ~/myapp/main.daml --daml --prefixes "Daml DA MyApp Test" --titles --border-top --border-bottom
```

via stdin

```
cat ~/myapp/main.daml | rejig --stdin --daml --prefixes "Daml DA MyApp Test" --titles --border-top --border-bottom
```

## Args


## `--file | --stdin`
```
(--file FILEPATH | --stdin DESCRIPTION)
```

`rejig` can read from a `--file` using the full path or pipe input using `--stdin` along with a description such as filename to make error reporting clearer.

## `--haskell | --daml`

input source language

## `--prefixes`
```
--prefixes "DA DA.Next DA.Finance Daml MyApp Test"
```

`--prefixes` enables user defined groupings of imports. Without this flag, the majority of the imports would all fall into the same group which may or may not be desirable.

**Important**: prefixes must be separated by whitespace and the most specific prefix listed from right to left.

Consider a `daml` project, based on the above `--prefixes`, the formatted result would as follows.

Note: the group titles are enabled via `--titles`

```
-- standard imports

import Something.Utils

-- imports by DA*

import DA.Action

import DA.Optional qualified as O

-- imports by DA.Finance*

import DA.Finance.Asset
import DA.Finance.Asset.Settlement

-- imports by DA.Next*

import DA.Next.Map qualified as M
import DA.Next.Set qualified as S

-- imports by Daml*

import Daml.Script
import Daml.Trigger

-- imports by MyApp*

import MyApp.Controller
import MyApp.Model

-- imports by Test*

import Test.App.Base
import Test.App.Helpers

```

## `--titles`

adds a single comment description above each primary category that `rejig` defines.

Each unique prefix group receives its own grouping + title, otherwise the import will be in the standard *"catch all"* import group unless its package qualified.

```
-- standard imports
-- imports by ${PREFIX}
-- package qualified
```

## `--border-top`

For visual purposes, an 80 character dashed comment is inserted before the first import if one exists.

```
module My.App
where

--------------------------------------------------------------------------------

-- standard imports
...
```

combined with `--border-bottom`, frames the import section as a whole.

## `--border-bottom`

For visual purposes, an 80 character dashed comment is inserted after the last import if one exists.

```
module My.App
where

-- standard imports

...

--------------------------------------------------------------------------------

...rest of source code

```

combined with `--border-top`, frames the import section as a whole.

# FAQ






# Editor integration

## vscode

[rejig-vscode-extension](https://github.com/mjstewart/rejig-vscode-extension) connects to the underlying `rejig` cli available on the `$PATH`. A command is registered and made available within `haskell` or `daml` files.

1. invoke the command pallet `ctrl+shift+p`
2. `>Rejig Document`

Any errors are written to `rejig-errors.txt` in workspace root.


# Formatting rules

Lets review what a module header is by looking at the example snippet below which explains how `rejig` works.

As we read the module header top down, observe that `rejig` sorts and groups declarations from the simplest structure to most complex based on a few simple heuristics for determining a complex vs simple import etc. Typically a complex import would use `as`, `qualified` and include a list of specific import things.

Notice that each grouping is sorted as deep as possible (pragmas included).

Regarding support for comments: they're only supported where mentioned to simplify matters and cater for things like legal disclaimers often found before the header declaration. `rejig` takes the ownership of managing any comments it creates through its various flags such as `--titles`.

Note: The below snippet is annotated with many comments for explanation and not included in `rejig` output.

```
  -- comments

  -- pragmas (OPTIONS_GHC must be first)

  {-# OPTIONS_GHC -Wno-alice #-}
  {-# OPTIONS_GHC -Wno-bob #-}

  -- comments

  {-# LANGUAGE Alice #-}
  {-# LANGUAGE Bob #-}

  -- comments

  module My.Special.App where
    ( alice            -- functions
    , bob
    , (<$>)            -- symbols
    , (<$$>)
    , Alice            -- constructor/type
    , Bob
    , Alice (..)       -- contructor/type + everything
    , Bob (..)
    , Alice ()         -- constructor/type + no things
    , Bob ()
    , Alice            -- constructor/type with things
        ( alice
        , bob
        , (<$>)
        , (<$$>)
        , Alice
        , Bob
        )
    , Bob
        ( alice
        , bob
        , (<$>)
        , (<$$>)
        , Alice
        , Bob
        )
    , module Alice      -- modules
    , module Bob
    )

-- constructor/type

import My.Special.App.Alice
import My.Special.App.Bob

-- constructor/type + no things

import My.Special.App.Alice ()
import My.Special.App.Bob ()

-- as

import My.Special.App.Alice as Alice
import My.Special.App.Bob as Bob

-- qualified

import qualified My.Special.App.Alice
import qualified My.Special.App.Bob

-- qualified + as

import qualified My.Special.App.Alice as Alice
import qualified My.Special.App.Bob as Alice

-- qualified + as + hiding

import qualified My.Special.App.Alice as Alice hiding
  ( alice
  , bob
  ...rest
  )
import qualified My.Special.App.Bob as Bob hiding
  ( alice
  , bob
  ...rest
  )

-- qualified + as + (no hiding)

import qualified My.Special.App.Alice as Alice
  ( alice
  , bob
  ...rest
  )
import qualified My.Special.App.Bob as Bob
  ( alice
  , bob
  ...rest
  )

-- package qualified import

import "alice" Another.App.Alice
... rest follows the same import ordering as above

import "bob" Another.App.Bob
... rest follows the same import ordering as above
```

# Sorting structure

top level groups

```

1. constructor/type
import My.Special.App.Alice

2. constructor/type + no things
import My.Special.App.Alice ()

3. as
import My.Special.App.Alice as Alice

4. qualified
import qualified My.Special.App.Alice

5. qualified + as
import qualified My.Special.App.Alice as Alice

6. qualified + as + hiding
import qualified My.Special.App.Alice as Alice hiding
  ( alice
  , bob
  ...rest
  )

7. qualified + as + (no hiding)
import qualified My.Special.App.Alice as Alice
  ( alice
  , bob
  ...rest
  )

8. package qualified import
import "alice" Another.App.Alice

package qualified imports apply the same ordering as the above steps, except all declarations are grouped under the package qualifier.

```

Within each top level group above, a nested subsort is performed using the following orderings

```
    ( alice            -- functions
    , bob
    , (<$>)            -- symbols
    , (<$$>)
    , Alice            -- constructor/type
    , Bob
    , Alice (..)       -- contructor/type + everything
    , Bob (..)
    , Alice ()         -- constructor/type + no things
    , Bob ()
    , Alice            -- constructor/type with things
        ( alice        -- (repeats same ordering rules)
        , bob
        , (<$>)
        , (<$$>)
        , Alice
        , Bob
        )
    , module Alice
    , module Bob
    )
```

# Getting started









```

echo "hi" | rejig --stdin

rejig --file "a.hs" > a.hs

```

# FAQ

errors

daml v1.6 only



# Implementation

This tool is implemented in `haskell` using [megaparsec](https://hackage.haskell.org/package/megaparsec) to parse "just enough" of the module headers defined in the haskell 98 grammar https://www.haskell.org/onlinereport/syntax-iso.html

haskell parsers already exist so why write another one? Primarily for education into parsers and bigger plans to parse `daml` which won't work with a `haskell` parser.
