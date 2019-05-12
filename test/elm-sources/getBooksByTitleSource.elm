module GetBooksByTitleSource exposing (..)

import Http
import Url.Builder
import Json.Decode as J

type alias Book = {}
jsonDecBook = J.succeed {}

getBooksByTitle : String -> Http.Request Book
getBooksByTitle capture_title =
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
                    [ "books"
                    , capture_title
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson <| jsonDecBook
            , timeout =
                Nothing
            , withCredentials =
                False
            }
