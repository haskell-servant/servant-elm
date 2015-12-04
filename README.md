# Servant Elm

Generate Elm functions to query your Servant API!

Elm type generation coutesy of [krisajenkins/elm-export](https://github.com/krisajenkins/elm-export).

## Example

Let's get some boring language pragmas and imports out of the way.

```haskell
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

import           Data.Proxy   (Proxy(Proxy))
import           Elm          (Spec(Spec), ToElmType, specsToDir)
import           GHC.Generics (Generic)
import           Servant      ((:>), Capture, Get, JSON)
import           Servant.Elm  (defElmImports, generateElmForAPI)
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
main = specsToDir "my-elm-dir" [spec]
```

Let's save this as `example.hs` and run it:

```
$ stack runghc example.hs
Writing: my-elm-dir/Generated/BooksApi.elm
$
```

Here's what was generated:

```elm
module Generated.BooksApi where

import Json.Decode exposing (..)
import Json.Decode.Extra exposing (apply)
import Json.Encode as JS
import Http
import String
import Task


type alias Book =
  {name : String}

decodeBook : Decoder Book
decodeBook = Book
  `map`   ("name" := string)

getBooksBy : Int -> Task.Task Http.Error (Book)
getBooksBy bookId =
  let request =
        { verb = "GET"
        , headers = [("Content-Type", "application/json")]
        , url = "/" ++ "books"
             ++ "/" ++ (bookId |> toString |> Http.uriEncode)
        , body = Http.empty
        }
  in  Http.fromJson
        decodeBook
        (Http.send Http.defaultSettings request)
```

See [`examples`](examples) for a complete usage example, or take a look at
https://github.com/mattjbray/servant-elm-example-app for an example project
using this library.

## Development

```
$ git clone https://github.com/mattjbray/servant-elm.git
$ cd servant-elm
$ stack build
$ stack test
```

## TODO

Servant API coverage:

* MatrixFlag / MatrixParam / MatrixParams
* Header (request)
* Headers (response)
* Delete / Patch / Put / Raw
* Vault / RemoteHost / IsSecure

Other:

* Option to not use elm-export: generate functions that take a decoder and
  String arguments.
