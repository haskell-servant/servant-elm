module PostBooksSource exposing (..)

import Http
import Url.Builder
import Json.Encode as Enc

type alias Book = {}
jsonEncBook = \b -> Enc.object []

postBooks : Book -> (Result Http.Error  (())  -> msg) -> Cmd msg
postBooks body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "books"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncBook body)
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
