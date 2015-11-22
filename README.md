Requires servant 0.5 (not yet released).

```
$ git clone https://github.com/mattjbray/servant-elm.git
$ cd servant-elm
$ stack build
```

## Example

Given some Haskell defining your types and Servant API:

```haskell
data Book = Book
  { name :: String
  } deriving (Generic)

instance ToElmType Book

type BooksApi = "books" :> Capture "bookId" Int :> Get '[JSON] Book
```

We can generate Elm functions to query the API:

```elm
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
