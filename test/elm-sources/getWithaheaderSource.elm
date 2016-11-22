module GetWithAHeaderSource exposing (..)

import Http
import Json.Decode exposing (..)


getWithaheader : String -> Int -> Http.Request (String)
getWithaheader myStringHeader myIntHeader =
    Http.request
        { method =
            "GET"
        , headers =
            [ Http.header "Content-Type" "application/json"
            , Http.header "myStringHeader" myStringHeader
            , Http.header "myIntHeader" (toString myIntHeader)
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
