# barlow-lens

Barlow lens increases your magnification and let's you see the stars sparkles

In other words, barlow lens makes creating complex lenses such as record lenses super simple.

This package is a port of [purescript-barlow-lens](https://github.com/sigma-andex/purescript-barlow-lens) based on [generic-lens](https://hackage.haskell.org/package/generic-lens).

## tl;dr

<!-- FOURMOLU_DISABLE -->

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
```

<!-- D

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

E -->

<!-- FOURMOLU_ENABLE -->

```haskell
import Control.Lens ((%~), (&), (^.), (^..), (^?))
import Data.Char (toUpper)
import Data.Lens.Barlow
import GHC.Generics
```

## Features

Barlow creates optics for the following types:

- 🥇 [`Records`](#tldr)
- 📦🐈 [`Maybe`](#maybe)
- 🤷🏽‍♀️ [`Either`](#either)
- 📜 [`Traversables`](#traversables)
- 🎁 [`Newtype`](#newtype)
- 🤖 [`Data types`](#data-types)

### Records

`zodiac` ~ field @"zodiac"

```haskell
data AlphaRecord = AlphaRecord {alpha :: String} deriving (Generic, Show)
data VirgoRecord = VirgoRecord {virgo :: AlphaRecord} deriving (Generic, Show)
data ZodiacRecord = ZodiacRecord {zodiac :: VirgoRecord} deriving (Generic, Show)

sky :: ZodiacRecord
sky = ZodiacRecord{zodiac = VirgoRecord{virgo = AlphaRecord{alpha = "Spica"}}}

spica :: String
spica = sky ^. (bw @"zodiac.virgo.alpha")

-- >>> spica
-- "Spica"

-- >>> alfa = sky ^. barlow @"zodiac.virgo.alfa"
-- The type AlphaRecord does not contain a field named 'alfa'.
-- In the second argument of `(^.)', namely
--   `barlow @"zodiac.virgo.alfa"'
-- In the expression: sky ^. barlow @"zodiac.virgo.alfa"
-- In an equation for `alfa':
--     alfa = sky ^. barlow @"zodiac.virgo.alfa"
```

### Maybe

Use `?` to zoom into a `Maybe`.

- `?` ~ `_Just :: Prism (Maybe a) (Maybe b) a b`

```haskell
newtype AlphaMaybe = AlphaMaybe {alpha :: Maybe String} deriving (Generic, Show)
newtype VirgoMaybe = VirgoMaybe {virgo :: Maybe AlphaMaybe} deriving (Generic, Show)
newtype ZodiacMaybe = ZodiacMaybe {zodiac :: Maybe VirgoMaybe} deriving (Generic, Show)

skyMaybe :: ZodiacMaybe
skyMaybe = ZodiacMaybe{zodiac = Just VirgoMaybe{virgo = Just AlphaMaybe{alpha = Just "Spica"}}}

spicaMaybe :: Maybe String
spicaMaybe = skyMaybe ^? bw @"zodiac?.virgo?.alpha?"

-- >>> spicaMaybe
-- Just "Spica"
```

### Either

Use `<` for `Left` and `>` for `Right` to zoom into an `Either`.

- `<` ~ `_Left :: Prism (Either a c) (Either b c) a b`
- `>` ~ `_Right :: Prism (Either c a) (Either c b) a b`

```haskell
newtype AlphaLeft = AlphaLeft {alpha :: Either String ()} deriving (Generic, Show)
newtype VirgoRight = VirgoRight {virgo :: Either () AlphaLeft} deriving (Generic, Show)
newtype ZodiacEither = ZodiacEither {zodiac :: Either VirgoRight VirgoRight} deriving (Generic, Show)

skyLeft :: ZodiacEither
skyLeft = ZodiacEither{zodiac = Left VirgoRight{virgo = Right AlphaLeft{alpha = Left "Spica"}}}

starLeftRightLeft :: Maybe String
starLeftRightLeft = skyLeft ^? bw @"zodiac<virgo>alpha<"

-- >>> starLeftRightLeft
-- Just "Spica"

starLeftLeft :: Maybe VirgoRight
starLeftLeft = skyLeft ^? bw @"zodiac>"

-- >>> starLeftLeft
-- Nothing
```

### Traversables

Use `+` to zoom into `Traversable`s.

- `+` ~ `traversed :: Traversable f => IndexedTraversal Int (f a) (f b) a b`

```haskell
newtype AlphaLeftRight = AlphaLeftRight {alpha :: Either String String} deriving (Generic, Show)
newtype VirgoLeftRight = VirgoLeftRight {virgo :: Either AlphaLeftRight AlphaLeftRight} deriving (Generic, Show)
newtype ZodiacList = ZodiacList {zodiac :: [VirgoLeftRight]} deriving (Generic, Show)

skyList :: ZodiacList
skyList =
  ZodiacList
    { zodiac =
        [ VirgoLeftRight{virgo = Right AlphaLeftRight{alpha = Left "Spica1"}}
        , VirgoLeftRight{virgo = Right AlphaLeftRight{alpha = Right "Spica2"}}
        , VirgoLeftRight{virgo = Left AlphaLeftRight{alpha = Right "Spica3"}}
        , VirgoLeftRight{virgo = Left AlphaLeftRight{alpha = Left "Spica4"}}
        ]
    }

starList :: [String]
starList = skyList ^.. bw @"zodiac+virgo>alpha>" & bw @"++" %~ toUpper

-- >>> starList
-- ["SPICA2"]

alphaRight :: [AlphaLeftRight]
alphaRight = skyList ^.. bw @"zodiac+virgo>"

-- >>> alphaRight
-- [AlphaLeftRight {alpha = Left "Spica1"},AlphaLeftRight {alpha = Right "Spica2"}]
```

### Newtype

Use `!` to zoom into a `newtype`.

- `!` ~ `wrappedIso :: Iso s t a b`

```haskell
newtype AlphaNewtype = AlphaNewtype {alpha :: String} deriving (Generic)
newtype VirgoNewtype = VirgoNewtype {virgo :: AlphaNewtype} deriving (Generic)
newtype ZodiacNewtype = ZodiacNewtype {zodiac :: VirgoNewtype} deriving (Generic)

skyNewtype :: ZodiacNewtype
skyNewtype = ZodiacNewtype (VirgoNewtype (AlphaNewtype "Spica"))

starNewtype :: [Char]
starNewtype = skyNewtype ^. bw @"zodiac!!"

-- >>> starNewtype
-- "Spica"
```

### Data types

Barlow supports zooming into arbitrary sum and product types as long as there is a `Generic` instance.

Use `%<NAME>` to zoom into sum types, where `<NAME>` is the name of your data constructor. E.g. `%VirgoData` for the data constructor `VirgoData`.

Use `%<INDEX>` to zoom into product types, where `<INDEX>` is a natural number.
Note that counting for product types and tuples usually starts with 1 and not 0.
So the first element of a product is `%1`.

It is more readable if you separate your sum lens from your product lens with a `.` dot.

- `%<NAME>` ~ `_Ctor :: AsConstructor ctor s t a b => Prism s t a b`
- `%<INDEX>` ~ `position :: HasPosition i s t a b => Lens s t a b`

```haskell
data ZodiacData
  = CarinaData {alpha :: String}
  | VirgoData {alpha :: String, beta :: String, gamma :: String, delta :: String}
  | CanisMaiorData String
  deriving (Generic)

skyData :: ZodiacData
skyData = VirgoData{alpha = "Spica", beta = "Beta Vir", gamma = "Gamma Vir", delta = "Del Vir"}

starData :: [Char]
starData = skyData ^. bw @"%VirgoData%3"

-- >>> starData
-- "Gamma Vir"
```

## Prerequisites

<details>

  <summary>Spoiler</summary>

- [flake.nix](./flake.nix) - code in this flake is extensively commented.
- [codium-haskell](https://github.com/deemp/flakes/tree/main/templates/codium/haskell#readme) - this flake.
- [codium-haskell-simple](https://github.com/deemp/flakes/tree/main/templates/codium/haskell-simple#readme) - a simplified version of this flake.
- [language-tools/haskell](https://github.com/deemp/flakes/blob/main/language-tools/haskell/flake.nix) - a flake that conveniently provides `Haskell` tools.
- [Conventions](https://github.com/deemp/flakes/blob/main/README/Conventions.md#dev-tools)
- [codium-generic](https://github.com/deemp/flakes/tree/main/templates/codium/generic#readme) - info just about `VSCodium` with extensions.
- [Haskell](https://github.com/deemp/flakes/blob/main/README/Haskell.md) - general info about `Haskell` tools.
- [Troubleshooting](https://github.com/deemp/flakes/blob/main/README/Troubleshooting.md)
- [Prerequisites](https://github.com/deemp/flakes#prerequisites)
- [Nixpkgs support for incremental Haskell builds](https://www.haskellforall.com/2022/12/nixpkgs-support-for-incremental-haskell.html)
- [flakes](https://github.com/deemp/flakes#readme) - my Nix flakes that may be useful for you.

</details>

## Quick start

1. Install Nix - see [how](https://github.com/deemp/flakes/blob/main/README/InstallNix.md).

1. In a new terminal, start a devshell, build and test the app.

    ```console
    nix develop
    cabal build
    cabal test
    ```

1. Write `settings.json` and start `VSCodium`.

    ```console
    nix run .#writeSettings
    nix run .#codium .
    ```

1. Open a `Haskell` file `app/Main.hs` and hover over a function.

1. Wait until `Haskell Language Server` (`HLS`) starts giving you type info.

1. Sometimes, `cabal` doesn't use the `Nix`-supplied packages ([issue](https://github.com/NixOS/nixpkgs/issues/130556#issuecomment-1114239002)). In this case, use `cabal v1-*` - commands.

## Configs

- [package.yaml](./package.yaml) - used by `stack` or `hpack` to generate a `.cabal`
- [.markdownlint.jsonc](./.markdownlint.jsonc) - for `markdownlint` from the extension `davidanson.vscode-markdownlint`
- [.ghcid](./.ghcid) - for [ghcid](https://github.com/ndmitchell/ghcid)
- [.envrc](./.envrc) - for [direnv](https://github.com/direnv/direnv)
- [fourmolu.yaml](./fourmolu.yaml) - for [fourmolu](https://github.com/fourmolu/fourmolu#configuration)
- [ci.yaml](.github/workflows/ci.yaml) - a generated `GitHub Actions` workflow. See [workflows](https://github.com/deemp/flakes/tree/main/workflows). Generate a workflow via `nix run .#writeWorkflows`.
- [hie.yaml](./hie.yaml) - a config for [hie-bios](https://github.com/haskell/hie-bios). Can be generated via [implicit-hie](https://github.com/Avi-D-coder/implicit-hie) to check the `Haskell Language Server` setup.
