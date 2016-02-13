module Generated.BooksApi where

import Json.Decode
import Json.Decode.Extra
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

postBooks : Book -> Task.Task Http.Error (Book)
postBooks body =
  let request =
        { verb = "POST"
        , headers = [("Content-Type", "application/json")]
        , url = "http://localhost:8000"
             ++ "/" ++ "books"
        , body = Http.string (Json.Encode.encode 0 (encodeBook body))
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
        (Json.Decode.list decodeBook)
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