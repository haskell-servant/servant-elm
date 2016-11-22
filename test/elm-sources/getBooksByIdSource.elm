module GetBooksByIdSource exposing (..)

import Http


getBooksById : Int -> Http.Request (Book)
getBooksById id =
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
                , id |> toString |> Http.encodeUri
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
