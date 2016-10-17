module Generated.BooksApi exposing (..)

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

encodeBook : Book -> Json.Encode.Value
encodeBook x =
    Json.Encode.object
        [ ( "name", Json.Encode.string x.name )
        ]

postBooks : Book -> Task.Task Http.Error (Book)
postBooks body =
  let
    request =
      { verb =
          "POST"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "http://localhost:8000"
          ++ "/" ++ "books"
      , body =
          Http.string (Json.Encode.encode 0 (encodeBook body))
      }
  in
    Http.fromJson
      decodeBook
      (Http.send Http.defaultSettings request)

getBooks : Task.Task Http.Error (List Book)
getBooks =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "http://localhost:8000"
          ++ "/" ++ "books"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (list decodeBook)
      (Http.send Http.defaultSettings request)

getBooksByBookId : Int -> Task.Task Http.Error (Book)
getBooksByBookId bookId =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "http://localhost:8000"
          ++ "/" ++ "books"
          ++ "/" ++ (bookId |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeBook
      (Http.send Http.defaultSettings request)