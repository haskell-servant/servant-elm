# Servant Elm

[![Build Status](https://travis-ci.org/mattjbray/servant-elm.svg?branch=master)](https://travis-ci.org/mattjbray/servant-elm)

Generate Elm functions to query your Servant API!

Elm type generation coutesy of [krisajenkins/elm-export](https://github.com/krisajenkins/elm-export).

## Installation

Until `elm-export` is released, `servant-elm` requires stack. Add this to your
`stack.yaml` file:

```yaml
...
packages:
  ...
- location:
    git: https://www.github.com/mattjbray/elm-export
    commit: a8a5b61798fbb04e081f5c83cab76ceaabc5ba13
  extra-dep: True
- location:
    git: https://www.github.com/mattjbray/servant-elm
    commit: 0cfa159f2767701d3f9171206c0806da43ce69be
  extra-dep: True
...
extra-deps:
- servant-0.5
- servant-foreign-0.5
- servant-server-0.5
```

## Example

Let's get some language pragmas and imports out of the way.

```haskell
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

import           GHC.Generics (Generic)
import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Elm  (Proxy (Proxy), Spec (Spec), ToElmType,
                               defElmImports, generateElmForAPI, specsToDir,
                               specsToDir)
```

We have some Haskell-defined types and our Servant API.

```haskell
data Book = Book
  { name :: String
  } deriving (Generic)

instance ToElmType Book

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

getBooksBy : Int -> Task.Task Http.Error (Book)
getBooksBy bookId =
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
$ stack build -flag servant-elm:examples
$ stack test
```

## TODO

* Encode captures and query params?
* Option to not use elm-export: generate functions that take a decoder and
  String arguments.
