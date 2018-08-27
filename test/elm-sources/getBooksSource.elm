module GetBooksSource exposing (..)

import Http
import Json.Decode exposing (..)
import Url


getBooks : Bool -> Maybe (String) -> Maybe (Int) -> String -> List (Bool) -> Http.Request (List (Book))
getBooks query_published query_sort query_year query_category query_filters =
    let
        params =
            List.filter (not << String.isEmpty)
                [ if query_published then
                    "published="
                  else
                    ""
                , query_sort
                    |> Maybe.map (identity >> Url.percentEncode >> (++) "sort=")
                    |> Maybe.withDefault ""
                , query_year
                    |> Maybe.map (String.fromInt >> Url.percentEncode >> (++) "year=")
                    |> Maybe.withDefault ""
                , Just query_category
                    |> Maybe.map (Url.percentEncode >> (++) "category=")
                    |> Maybe.withDefault ""
                , query_filters
                    |> List.map (\val -> "filters[]=" ++ (val |> (\v -> if v then "True" else "False") |> Url.percentEncode))
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
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson (list decodeBook)
            , timeout =
                Nothing
            , withCredentials =
                False
            }
