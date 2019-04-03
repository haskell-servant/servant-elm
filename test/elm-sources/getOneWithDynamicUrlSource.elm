module GetOneWithDynamicUrlSource exposing (..)

import Http
import Json.Decode exposing (..)


getOne : String -> Http.Request Int
getOne urlBase =
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
