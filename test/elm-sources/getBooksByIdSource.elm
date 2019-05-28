module GetBooksByIdSource exposing (..)

import Http
import Url.Builder
import Json.Decode

type Book = Book
jsonDecBook : Json.Decode.Decoder Book
jsonDecBook = Debug.todo ""

getBooksById : Int -> (Result Http.Error  (Book)  -> msg) -> Cmd msg
getBooksById capture_id toMsg =
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
                    , capture_id |> String.fromInt
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
