module GetBooksByTitleSource exposing (..)

import Http
import Url.Builder
import Json.Decode as J

type alias Book = {}
jsonDecBook = J.succeed {}

getBooksByTitle : String -> (Result Http.Error  (Book)  -> msg) -> Cmd msg
getBooksByTitle capture_title toMsg =
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
                    , capture_title
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
