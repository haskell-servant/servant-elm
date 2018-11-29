module Generated.BooksApi exposing (..)

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

encodeBook : Book -> Json.Encode.Value
encodeBook x =
    Json.Encode.object
        [ ( "name", Json.Encode.string x.name )
        ]

postBooks : Book -> Http.Request (Http.Response (Book))
postBooks body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8000"
                , "books"
                ]
        , body =
            Http.jsonBody (encodeBook body)
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

getBooks : Http.Request (Http.Response (List (Book)))
getBooks =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8000"
                , "books"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\res ->
                    Result.mapError Json.Decode.errorToString
                        (Result.map
                            (\body_ -> { url = res.url, status = res.status, headers = res.headers, body = body_ })
                            (decodeString (list decodeBook) res.body)))
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getBooksByBookId : Int -> Http.Request (Http.Response (Book))
getBooksByBookId capture_bookId =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8000"
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
