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