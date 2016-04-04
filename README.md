# Servant Elm

[![Build Status](https://travis-ci.org/mattjbray/servant-elm.svg?branch=master)](https://travis-ci.org/mattjbray/servant-elm)

Generate Elm functions to query your Servant API!

Elm type generation coutesy of [krisajenkins/elm-export](https://github.com/krisajenkins/elm-export).

## Installation

Until `elm-export` is released, `servant-elm` requires stack. Add this to your
`stack.yaml` file:

```yaml
...
resolver: lts-5.10

packages:
- '.'
- location:
    git: https://www.github.com/mattjbray/elm-export
    commit: 3dfafc7a717003ff4374119ff6f60e5b56868d8f
  extra-dep: True
- location:
    git: https://www.github.com/mattjbray/servant-elm
    commit: 36c90557d17d237e621cdcb4912ae9e4f25a9e59
  extra-dep: True

extra-deps:
- servant-0.5
- servant-foreign-0.5
- servant-server-0.5
```

## Example

First, some language pragmas and imports.

```haskell
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

import           GHC.Generics (Generic)
import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Elm  (ElmType, Proxy (Proxy), Spec (Spec),
                               defElmImports, generateElmForAPI, specsToDir,
                               specsToDir)
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
             : generateElmForAPI (Proxy :: Proxy BooksApi))

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
module Generated.MyApi where

import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Http
import String
import Task


type alias Book =
  { name : String
  }

decodeBook : Json.Decode.Decoder Book
decodeBook =
  Json.Decode.succeed Book
    |: ("name" := Json.Decode.string)

encodeBook : Book -> Json.Encode.Value
encodeBook x =
  Json.Encode.object
    [ ( "name", Json.Encode.string x.name )
    ]

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
$ stack build --flag servant-elm:examples
$ stack test
```

## TODO

* Encode captures and query params?
* Option to not use elm-export: generate functions that take a decoder and
  String arguments.
