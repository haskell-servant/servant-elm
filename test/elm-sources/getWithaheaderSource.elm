module GetWithAHeaderSource exposing (..)

import Http
import Json.Decode exposing (..)


getWithaheader : String -> Int -> Http.Request (String)
getWithaheader header_myStringHeader header_MyIntHeader =
    Http.request
        { method =
            "GET"
        , headers =
            [ Http.header "myStringHeader" header_myStringHeader
            , Http.header "MyIntHeader" (toString header_MyIntHeader)
            ]
        , url =
            String.join "/"
                [ ""
                , "with-a-header"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson string
        , timeout =
            Nothing
        , withCredentials =
            False
        }
