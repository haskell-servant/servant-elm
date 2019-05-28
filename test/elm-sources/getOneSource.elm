module GetOneSource exposing (..)

import Http
import Json.Decode exposing (..)
import Url.Builder


getOne : (Result Http.Error  (Int)  -> msg) -> Cmd msg
getOne toMsg =
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
                Url.Builder.crossOrigin ""
                    [ "one"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg Json.Decode.int
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
