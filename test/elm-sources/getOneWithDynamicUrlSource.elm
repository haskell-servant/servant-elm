module GetOneWithDynamicUrlSource exposing (..)

import Http
import Json.Decode exposing (..)


getOne : String -> Http.Request (Int)
getOne urlBase =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
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
