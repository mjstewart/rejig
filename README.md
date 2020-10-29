# Rejig

***organise (something) differently; rearrange.***

A *"module header"* (import / export declarations) formatting tool for `haskell` and *[daml](https://github.com/digital-asset/daml).

This tool provides an opinionated strategy for grouping and deep sorting import/export declarations by *structural components* instead of simple alphabetical module names.

`rejig` doesn't attempt to be a code formatter, it simply formats the module header section and leaves the rest of the source code unchanged.

\* supports `daml version >= 1.6.0`

![demo](https://github.com/mjstewart/rejig-vscode-extension/blob/master/rejig-vscode-sample.gif)


# Installation

`rejig` is a cli tool and requires the binary to be on the `$PATH`.

### 1. Download binary

https://github.com/mjstewart/rejig/releases

### 2. Add to path

Use your preferred method of adding `rejig` to the `$PATH`

Here's an example of my linux setup.

```
-- I keep user apps in /opt so I move the download there.
mv ~/Downloads/rejig /opt

-- Ensure its executable
chmod +x /opt/rejig

-- symlink so its available somewhere on $PATH
ln -s -f /opt/rejig /usr/bin/rejig
```

I don't use windows so I don't know if this will work, the binary *"should"* work if invoked using `rejig.exe`.

# Editor integration

## vscode

[rejig-vscode-extension](https://github.com/mjstewart/rejig-vscode-extension)

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

## typical cli examples

via file
```
rejig --file ~/myapp/main.daml --daml --prefixes "Daml DA MyApp.Main MyApp.Test" --titles --border-top --border-bottom
```

via stdin

```
cat ~/myapp/main.daml | rejig --stdin --daml --prefixes "Daml DA MyApp.Main MyApp.Test" --titles --border-top --border-bottom
```

## Args


## `--file | --stdin`
```
(--file FILEPATH | --stdin DESCRIPTION)
```

Supply `rejig` with the file path or pipe input using `--stdin` along with a description such as filename to make error reporting clearer.

## `--haskell | --daml`

input source language

## `--prefixes`
```
--prefixes "DA DA.Next DA.Finance Daml MyApp.Main MyApp.Test"
```

`--prefixes` enables custom import groups. Without this flag, the majority of the imports would all fall into the same group which may or may not be desirable.

**Important**: prefixes must be separated by whitespace and the most specific prefix listed from right to left.

To get a feel for how this looks, consider a `daml` project. If using the above `--prefixes`, `rejig` would output something similar to the following snippet.

Note: the group titles are enabled via `--titles`

```
-- standard imports

import Something.Base
import Something.Core

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

-- imports by MyApp.Main*

import MyApp.Main.Controller
import MyApp.Main.Model

-- imports by MyApp.Test*

import MyApp.Test.Base
import MyApp.Test.Helpers

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

combined with `--border-bottom`, frames the import section as a whole.

```
module My.App
where

--------------------------------------------------------------------------------

-- standard imports

import Alice
import Bob

```

## `--border-bottom`

For visual purposes, an 80 character dashed comment is inserted after the last import if one exists.

combined with `--border-top`, frames the import section as a whole.

```
module My.App
where

-- standard imports

import Alice
import Bob

--------------------------------------------------------------------------------

... rest of source
```

# Formatting rules

Lets review what a module header is by looking at the example snippet below which explains how `rejig` works.

As we read the module header top down, observe that `rejig` sorts and groups declarations from the simplest structure to most complex based on a few simple heuristics for determining a complex vs simple import etc. Typically a complex import would use `as`, `qualified` and include a list of specific import things.

Notice that each grouping is sorted as deep as possible (pragmas included).

Regarding support for comments: they're only supported where mentioned to simplify matters and cater for things like legal disclaimers often found before the header declaration. `rejig` takes the ownership of managing any comments it creates through its various flags such as `--titles`.

Note: The below snippet is annotated with many comments for explanation and may not be included in `rejig` output.

```
  -- comments

  -- pragmas (OPTIONS_GHC must be first)

  {-# OPTIONS_GHC -Wno-alice #-}
  {-# OPTIONS_GHC -Wno-bob #-}

  -- comments

  {-# LANGUAGE Alice #-}
  {-# LANGUAGE Bob #-}

  -- comments

  module MyApp where
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

import MyApp.Main.Alice
import MyApp.Main.Bob

-- constructor/type + no things

import MyApp.Main.Alice ()
import MyApp.Main.Bob ()

-- as

import MyApp.Main.Alice as Alice
import MyApp.Main.Bob as Bob

-- qualified

import qualified MyApp.Main.Alice
import qualified MyApp.Main.Bob

-- qualified + as

import qualified MyApp.Main.Alice as Alice
import qualified MyApp.Bob as Alice

-- qualified + as + hiding

import qualified MyApp.Main.Alice as Alice hiding
  ( alice
  , bob
  ...rest
  )
import qualified MyApp.Main.Bob as Bob hiding
  ( alice
  , bob
  ...rest
  )

-- qualified + as + (no hiding)

import qualified MyApp.Main.Alice as Alice
  ( alice
  , bob
  ...rest
  )
import qualified MyApp.Main.Bob as Bob
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
import MyApp.Main.Alice

2. constructor/type + no things
import MyApp.Main.Alice ()

3. as
import MyApp.Main.Alice as Alice

4. qualified
import qualified MyApp.Main.Alice

5. qualified + as
import qualified MyApp.Main.Alice as Alice

6. qualified + as + hiding
import qualified MyApp.Main.Alice as Alice hiding
  ( alice
  , bob
  ...rest
  )

7. qualified + as + (no hiding)
import qualified MyApp.Main.Alice as Alice
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

# FAQ

## 1. Are there any editor plugins?

[vscode](#Editor-Integration)

## 2. `daml` parse errors

`daml >= v1.6.0` is supported only

## 3. pragma parse errors

Only 2 pragmas are supported and `OPTIONS_GHC` must occur first.

  ```
  {-# OPTIONS_GHC -Wall #-}
  {-# LANGUAGE AllowAmbiguousTypes #-}
  ```

## 4. Disappearing comments

user defined comments can only exist before the module header or pragmas to cater for things like legal/copyright notices etc. Unexpected results will occur if comments are scattered within import / export declarations.

`rejig` automatically manages any comments it generates through its various flags such as `--titles`.

## 5. Nothing is being formatted

The parser implements the very basics of haskell 98 module header syntax in a very primitive way. This implies its very likely that not all syntax is supported and I don't provide any guarantees that it works 100% of the time.

The best advice I can give is to try limit the amount of comments since `rejig` will reorder code and potentially shift comments to somewhere that no longer make any sense.

Generally the issue will be relating to having random comments scattered within import statements. This causes the parser to exit early at the failure point and dump the rest of the source code unmodified.

Please create an issue if it looks like a bug and I'll happily take a look.

# Implementation

This tool is implemented in `haskell` using [megaparsec](https://hackage.haskell.org/package/megaparsec) to parse "just enough" of the module headers defined using the haskell 98 guide https://www.haskell.org/onlinereport/syntax-iso.html

If haskell parsers already exist, why did I write my own? Primarily for education because I find parsing interesting and I wanted to write one in `haskell` :),
there are some other technical reasons / bigger plans since I wanted to support `daml` too.
