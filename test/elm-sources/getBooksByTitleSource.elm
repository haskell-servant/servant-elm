module GetBooksByTitleSource exposing (..)

import Http
import Url.Builder
import Json.Decode as J

type alias Book = {}
jsonDecBook = J.succeed {}

getBooksByTitlespace : String -> (Result Http.Error  (Book)  -> msg) -> Cmd msg
getBooksByTitlespace capture_title_space toMsg =
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
                    [ "books"
                    , Url.percentEncode (capture_title_space)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecBook
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
