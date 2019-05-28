module GetOneWithDynamicUrlSource exposing (..)

import Http
import Url.Builder
import Json.Decode exposing (..)


getOne : String -> (Result Http.Error  (Int)  -> msg) -> Cmd msg
getOne urlBase toMsg =
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
                Http.expectJson toMsg Json.Decode.int
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
