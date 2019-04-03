module GetOneSource exposing (..)

import Http
import Json.Decode exposing (..)
import Url.Builder


getOne : Http.Request Int
getOne =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.absolute
                    [ "one"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson <| Json.Decode.int
            , timeout =
                Nothing
            , withCredentials =
                False
            }
