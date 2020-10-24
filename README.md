# Rejig

***organise (something) differently; rearrange.***

A "module header" (import / export declarations) formatting tool for `haskell` and [daml](https://github.com/digital-asset/daml).

This tool provides an opinionated strategy for grouping and deep sorting import/export declarations by *structural components*. Contrast this to other code formatters that tend to perform a simple top level ordering by alphabetical module name only.

`rejig` isn't invasive and doesn't attempt to be a code formatter. It simply formats the module header section and leaves the rest of the source code unchanged. Its designed for use with `daml` in mind which cant leverage existing haskell formatters since the languages aren't exactly 1 to 1.

# Installation

`rejig` is a cli tool and requires the binary to be on the `$PATH`.

### 1. Download binary
https://github.com/mjstewart/rejig/releases

### 2. Add to path

Add `rejig` to your `$PATH` however you prefer.

Here's an example of my linux setup.

```
-- I keep user apps in /opt so I move the download there.
mv ~/Downloads/rejig /opt

-- Ensure its executable
chmod +x /opt/rejig

-- symlink so its available somewhere on $PATH
ln -s -f /opt/rejig /usr/bin/rejig
```

I don't use windows so I don't know if this will work, the binary **should** work if invokved using `rejig.exe`.

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

## typical usage

via file
```
rejig --file ~/myapp/main.daml --daml --prefixes "DA MyApp Test" --titles --border-top --border-bottom
```

via stdin

```
cat ~/myapp/main.daml | rejig --stdin --daml --prefixes "DA MyApp Test" --titles --border-top --border-bottom
```

## Arg definitions

```
(--file FILEPATH | --stdin DESCRIPTION)
```
Either the full file path or when supplying via `stdin`, supply a description such as the name of the file to make error reporting clearer.

```
--prefixes "DA. DA.Lib.Finance MyApp Test"
```

`--prefixes` create new groupings for further clarity and ordering. Without this flag, the majority of the imports would all fall into the same group which may or may not be desirable.

Consider a `daml` project, I find it nice to group all the `DA` standard libs together, then my own namespace + tests.
Each new prefix group receives a descriptive title when `--titles` is enabled.

```

```

 then you'll also have your own module namespaces and tests too. Each prefix group receives their own title when `--titles` is enabled.

Prefixes must be separated by whitespace and the most specific prefix must be placed last.

If a module name matches one of these prefixes its placed into their own group rather than be lumped together with everything else.

The most specific import must be listed last, eg `DA.Lib.Finance` is more specific than `DA` so therefore `DA` is listed before as a 'catch all' type mechanism.


# FAQ






# Editor integration

## vscode

[rejig-vscode-extension](https://github.com/mjstewart/rejig-vscode-extension) connects to the underlying `rejig` cli available on the `$PATH`. A command is registered and made avalable within `haskell` or `daml` files.

1. invoke the command pallet `ctrl+shift+p`
2. `>Rejig Document`
3. document is formatted or errors are written to `rejig-errors.txt` in workspace root.


# Formatting rules

Lets review what a module header is by looking at the example snippet below which explains how `rejig` works.

As we read the module header top down, observe that `rejig` sorts and groups declarations from the simplest structure to most complex based on a few simple heuristics for determining a complex vs simple import etc. Typically a complex import would use `as`, `qualified` and include a list of specific import things.

Notice that each grouping is sorted as deep as possible (pragmas included).

Regarding support for comments: they're only supported where mentioned to simplify matters and avoids having comments scattered within import statements for example.

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
