module Generated.BooksApi exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String
import String.Conversions as String


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
                (\response ->
                    Result.map
                        (\body -> { response | body = body })
                        (decodeString decodeBook response.body))
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
                (\response ->
                    Result.map
                        (\body -> { response | body = body })
                        (decodeString (list decodeBook) response.body))
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
                , capture_bookId |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\response ->
                    Result.map
                        (\body -> { response | body = body })
                        (decodeString decodeBook response.body))
        , timeout =
            Nothing
        , withCredentials =
            False
        }
