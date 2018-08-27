module GetBooksByTitleSource exposing (..)

import Http
import Url


getBooksByTitle : String -> Http.Request (Book)
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
                , capture_title |> Url.percentEncode
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeBook
        , timeout =
            Nothing
        , withCredentials =
            False
        }
