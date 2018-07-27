module Generated.BooksApi exposing(..)

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
   ("name" := Json.Decode.string) >>= \pname ->
   Json.Decode.succeed {name = pname}

jsonEncBook : Book -> Value
jsonEncBook  val =
   Json.Encode.object
   [ ("name", Json.Encode.string val.name)
   ]


postBooks : Book -> Http.Request Book
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
            Http.jsonBody (jsonEncBook body)
        , expect =
            Http.expectJson <| jsonDecBook
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getBooks : Http.Request (List Book)
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
            Http.expectJson <| Json.Decode.list (jsonDecBook)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getBooksByBookId : Int -> Http.Request Book
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
            Http.expectJson <| jsonDecBook
        , timeout =
            Nothing
        , withCredentials =
            False
        }
