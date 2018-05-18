module Generated.MyApi exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String

type alias Book  =
   { name: String
   }

jsonDecBook : Json.Decode.Decoder ( Book )
jsonDecBook =
   (Json.Decode.string) >>= \pname ->
   Json.Decode.succeed {name = pname}

jsonEncBook : Book -> Value
jsonEncBook  val =
   Json.Encode.string val.name

getBooksByBookId : Int -> Http.Request Book
getBooksByBookId capture_bookId =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "books"
                , capture_bookId |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson <| jsonDecBook
        , timeout =
            Nothing
        , withCredentials =
            False
        }
