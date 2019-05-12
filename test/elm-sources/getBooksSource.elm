module GetBooksSource exposing (..)

import Http
import Json.Decode exposing (..)
import Url.Builder
import Json.Decode as J

type alias Book = {}

maybeBoolToIntStr : Maybe Bool -> String
maybeBoolToIntStr mx =
  case mx of
    Nothing -> ""
    Just True -> "1"
    Just False -> "0"

jsonDecBook = J.succeed {}

getBooks : Bool -> (Maybe String) -> (Maybe Int) -> String -> (List (Maybe Bool)) -> Http.Request (List Book)
getBooks query_published query_sort query_year query_category query_filters =
    let
        params =
            List.filterMap identity
            (List.concat
                [ [ if query_published then
                    Just (Url.Builder.string "published" "")
                  else
                    Nothing ]
                , [ query_sort
                    |> Maybe.map (Url.Builder.string "query_sort") ]
                , [ query_year
                    |> Maybe.map (String.fromInt >> Url.Builder.string "query_year") ]
                , [ Just query_category
                    |> Maybe.map (Url.Builder.string "query_category") ]
                , query_filters
                    |> List.map (\val -> Just (Url.Builder.string "filters[]" (maybeBoolToIntStr val)))
                ])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.absolute
                    [ "books"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson <| Json.Decode.list (jsonDecBook)
            , timeout =
                Nothing
            , withCredentials =
                False
            }
