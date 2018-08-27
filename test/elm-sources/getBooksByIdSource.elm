module GetBooksByIdSource exposing (..)

import Http
import Url


getBooksById : Int -> Http.Request (Book)
getBooksById capture_id =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "books"
                , capture_id |> String.fromInt |> Url.percentEncode
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
