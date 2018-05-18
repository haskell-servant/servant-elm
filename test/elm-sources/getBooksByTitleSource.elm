module GetBooksByTitleSource exposing (..)

import Http


getBooksByTitle : String -> Http.Request Book
getBooksByTitle capture_title =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "books"
                , capture_title |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson <| jsonDecBook
        , timeout =
            Nothing
        , withCredentials =
            False
        }
