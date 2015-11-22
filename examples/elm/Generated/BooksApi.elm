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

encodeBook : Book -> JS.Value
encodeBook x =
  JS.object
    [("name", JS.string x.name)]

postBooks : Book -> Task.Task Http.Error (Book)
postBooks body =
  let request =
        { verb = "POST"
        , headers = [("Content-Type", "application/json")]
        , url = "http://localhost:8000"
             ++ "/" ++ "books"
        , body = Http.string (JS.encode 0 (encodeBook body))
        }
  in  Http.fromJson
        decodeBook
        (Http.send Http.defaultSettings request)

getBooks : Task.Task Http.Error (List (Book))
getBooks =
  let request =
        { verb = "GET"
        , headers = [("Content-Type", "application/json")]
        , url = "http://localhost:8000"
             ++ "/" ++ "books"
        , body = Http.empty
        }
  in  Http.fromJson
        (list decodeBook)
        (Http.send Http.defaultSettings request)

getBooksBy : Int -> Task.Task Http.Error (Book)
getBooksBy bookId =
  let request =
        { verb = "GET"
        , headers = [("Content-Type", "application/json")]
        , url = "http://localhost:8000"
             ++ "/" ++ "books"
             ++ "/" ++ (bookId |> toString |> Http.uriEncode)
        , body = Http.empty
        }
  in  Http.fromJson
        decodeBook
        (Http.send Http.defaultSettings request)