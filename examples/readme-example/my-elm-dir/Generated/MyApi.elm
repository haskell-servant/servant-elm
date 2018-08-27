module Generated.MyApi exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String
import Url


type alias Book =
    { name : String
    }

decodeBook : Decoder Book
decodeBook =
    succeed Book
        |> required "name" string

getBooksByBookId : Int -> Http.Request (Book)
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
                , capture_bookId |> String.fromInt |> Url.percentEncode
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeBook
        , timeout =
            Nothing
        , withCredentials =
            False
        }