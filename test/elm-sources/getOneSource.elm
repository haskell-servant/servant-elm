module GetOneSource exposing (..)

import Http
import Json.Decode exposing (..)


getOne : Http.Request (Int)
getOne =
    Http.request
        { method =
            "GET"
        , headers =
            [ Http.header "Content-Type" "application/json"
            ]
        , url =
            String.join "/"
                [ ""
                , "one"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson int
        , timeout =
            Nothing
        , withCredentials =
            False
        }
