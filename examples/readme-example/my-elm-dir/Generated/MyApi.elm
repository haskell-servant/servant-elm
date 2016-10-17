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