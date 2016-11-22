module GetBooksByTitleSource exposing (..)

import Http


getBooksByTitle : String -> Http.Request (Book)
getBooksByTitle title =
    Http.request
        { method =
            "GET"
        , headers =
            [ Http.header "Content-Type" "application/json"
            ]
        , url =
            String.join "/"
                [ ""
                , "books"
                , title |> Http.encodeUri
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
