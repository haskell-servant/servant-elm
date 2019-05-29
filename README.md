# Servant Elm

[![Build Status](https://travis-ci.org/mattjbray/servant-elm.svg?branch=master)](https://travis-ci.org/mattjbray/servant-elm)

Generate Elm functions to query your Servant API!

Elm type generation coutesy of [elm-bridge](http://hackage.haskell.org/package/elm-bridge).

## Installation

Servant Elm is [available on Hackage](http://hackage.haskell.org/package/servant-elm).

## Example

First, some language pragmas and imports.

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

import           Elm.Derive   (defaultOptions, deriveBoth)

import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Elm  (DefineElm (DefineElm), Proxy (Proxy), defElmImports, defElmOptions,
                               generateElmModuleWith)
```

We have some Haskell-defined types and our Servant API.

```haskell
data Book = Book
    { name :: String
    }

deriveBoth defaultOptions ''Book

type BooksApi = "books" :> Capture "bookId" Int :> Get '[JSON] Book
```

Now we can generate Elm functions to query the API:

```haskell
main :: IO ()
main =
  generateElmModuleWith
    defElmOptions
    [ "Generated"
    , "MyApi"
    ]
    defElmImports
    "my-elm-dir"
    [ DefineElm (Proxy :: Proxy Book)
    ]
    (Proxy :: Proxy BooksApi)
```

Let's save this as `example.hs` and run it:

```
$ stack runghc example.hs
Writing: my-elm-dir/Generated/MyApi.elm
$
```

Here's what was generated:

```elm
module Generated.MyApi exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String
import Url.Builder

maybeBoolToIntStr : Maybe Bool -> String
maybeBoolToIntStr mx =
  case mx of
    Nothing -> ""
    Just True -> "1"
    Just False -> "0"

type alias Book  =
   { name: String
   }

jsonDecBook : Json.Decode.Decoder ( Book )
jsonDecBook =
   Json.Decode.succeed (\pname -> {name = pname})
   |> required "name" (Json.Decode.string)

jsonEncBook : Book -> Value
jsonEncBook  val =
   Json.Encode.object
   [ ("name", Json.Encode.string val.name)
   ]


getBooksByBookId : Int -> Http.Request Book
getBooksByBookId capture_bookId =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.absolute
                    [ "books"
                    , capture_bookId |> String.fromInt
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson <| jsonDecBook
            , timeout =
                Nothing
            , withCredentials =
                False
            }
```

See [`examples`](examples) for a complete usage example, or take a look at
[mattjbray/servant-elm-example-app](https://github.com/mattjbray/servant-elm-example-app)
for an example project using this library.

## Development

```
$ git clone https://github.com/mattjbray/servant-elm.git
$ cd servant-elm
$ stack test
$ stack test --flag servant-elm:integration
```

To build all examples:

```
$ make examples
```

To run an example:

```
$ cd examples/e2e-tests
$ elm-reactor
# Open http://localhost:8000/elm/Main.elm
```
