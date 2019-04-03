module GetBooksByTitleSource exposing (..)

import Http


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
