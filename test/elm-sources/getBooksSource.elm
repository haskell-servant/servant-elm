module GetBooksSource exposing (..)

import Http
import Json.Decode exposing (..)


getBooks : Bool -> Maybe String -> Maybe Int -> List (Maybe Bool) -> Http.Request (List Book)
getBooks query_published query_sort query_year query_filters =
    let
        params =
            List.filter (not << String.isEmpty)
                [ if query_published then
                    "query_published="
                  else
                    ""
                , query_sort
                    |> Maybe.map (identity >> Http.encodeUri >> (++) "sort=")
                    |> Maybe.withDefault ""
                , query_year
                    |> Maybe.map (String.fromInt >> Http.encodeUri >> (++) "year=")
                    |> Maybe.withDefault ""
                , query_filters
                    |> List.map (\val -> "query_filters[]=" ++ (val |> String.fromBool |> Http.encodeUri))
                    |> String.join "&"
                ]
    in
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "books"
                ]
                ++ (if List.isEmpty params then
                        ""
                    else
                        "?" ++ String.join "&" params
                   )
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeBook)
        , timeout =
            Nothing
        , withCredentials =
            False
        }
