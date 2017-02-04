module Generated.BooksApi exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


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

postBooks : Book -> Http.Request (Book)
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
            Http.expectJson decodeBook
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getBooks : Http.Request (List (Book))
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
            Http.expectJson (list decodeBook)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getBooksByBookId : Int -> Http.Request (Book)
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
            Http.expectJson decodeBook
        , timeout =
            Nothing
        , withCredentials =
            False
        }