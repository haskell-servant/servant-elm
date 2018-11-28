module GetBooksSource exposing (..)

import Http
import String.Conversions as String
import Json.Decode exposing (..)
import Url


getBooks : Bool -> Maybe (String) -> Maybe (Int) -> List (Maybe (Bool)) -> Http.Request (Http.Response (List (Book)))
getBooks query_published query_sort query_year query_filters =
    let
        params =
            List.filter (not << String.isEmpty)
                [ if query_published then
                    "query_published="
                  else
                    ""
                , query_sort
                    |> Maybe.map (Url.percentEncode >> (++) "sort=")
                    |> Maybe.withDefault ""
                , query_year
                    |> Maybe.map (String.fromInt >>Url.percentEncode >> (++) "year=")
                    |> Maybe.withDefault ""
                , query_filters
                    |> List.map (\val -> "query_filters[]=" ++ (val |> Maybe.map (String.fromBool) |> Maybe.withDefault "" |> Url.percentEncode))
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
                Http.expectStringResponse
                    (\res ->
                        Result.mapError Json.Decode.errorToString
                            (Result.map
                                (\body_ -> { url = res.url, status = res.status, headers = res.headers, body = body_ })
                                (decodeString (list decodeBook) res.body)))
            , timeout =
                Nothing
            , withCredentials =
                False
            }
