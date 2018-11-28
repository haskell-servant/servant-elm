module Generated.MyApi exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String.Conversions as String
import Url


type alias Book =
    { name : String
    }

decodeBook : Decoder Book
decodeBook =
    succeed Book
        |> required "name" string

getBooksByBookId : Int -> Http.Request (Http.Response (Book))
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
            Http.expectStringResponse
                (\res ->
                    Result.mapError Json.Decode.errorToString
                        (Result.map
                            (\body_ -> { url = res.url, status = res.status, headers = res.headers, body = body_ })
                            (decodeString decodeBook res.body)))
        , timeout =
            Nothing
        , withCredentials =
            False
        }
