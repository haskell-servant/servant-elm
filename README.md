# Servant Elm

[![Build Status](https://travis-ci.org/mattjbray/servant-elm.svg?branch=master)](https://travis-ci.org/mattjbray/servant-elm)

Generate Elm functions to query your Servant API!

Elm type generation coutesy of [krisajenkins/elm-export](https://github.com/krisajenkins/elm-export).

## Installation

`servant-elm` currently relies on a [forked version](https://github.com/mattjbray/elm-export) of `elm-export`. Until we can get the required features merged upstream, `servant-elm` requires stack. Add this to your
`stack.yaml` file:

```yaml
...
resolver: lts-7.2

packages:
- '.'
- location:
    git: https://www.github.com/mattjbray/elm-export
    commit: ae5aa012f17732d48b6f19ffc4a52bdbe8f42aba
  extra-dep: True
- location:
    git: https://www.github.com/mattjbray/servant-elm
    commit: 8854758c7f08f751e2f2628158ec3ae67c8b70c6
  extra-dep: True
```

## Example

First, some language pragmas and imports.

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Elm          (Spec (Spec), specsToDir, toElmDecoderSource,
                               toElmTypeSource)
import           GHC.Generics (Generic)
import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Elm  (ElmType, Proxy (Proxy), defElmImports,
                               generateElmForAPI)
```

We have some Haskell-defined types and our Servant API.

```haskell
data Book = Book
  { name :: String
  } deriving (Generic)

instance ElmType Book

type BooksApi = "books" :> Capture "bookId" Int :> Get '[JSON] Book
```

Now we can generate Elm functions to query the API:

```haskell
spec :: Spec
spec = Spec ["Generated", "MyApi"]
            (defElmImports
             : toElmTypeSource    (Proxy :: Proxy Book)
             : toElmDecoderSource (Proxy :: Proxy Book)
             : generateElmForAPI  (Proxy :: Proxy BooksApi))

main :: IO ()
main = specsToDir [spec] "my-elm-dir"
```

Let's save this as `example.hs` and run it:

```
$ stack runghc example.hs
Writing: my-elm-dir/Generated/MyApi.elm
$
```

Here's what was generated:

```elm
module Generated.MyApi exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String
import Task


type alias Book =
    { name : String
    }

decodeBook : Decoder Book
decodeBook =
    decode Book
        |> required "name" string

getBooksByBookId : Int -> Task.Task Http.Error (Book)
getBooksByBookId bookId =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "books"
          ++ "/" ++ (bookId |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeBook
      (Http.send Http.defaultSettings request)
```

See [`examples`](examples) for a complete usage example, or take a look at
[mattjbray/servant-elm-example-app](https://github.com/mattjbray/servant-elm-example-app)
for an example project using this library.

## Development

```
$ git clone https://github.com/mattjbray/servant-elm.git
$ cd servant-elm
$ stack test
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

## TODO

* Encode captures and query params?
* Option to not use elm-export: generate functions that take a decoder and
  String arguments.
