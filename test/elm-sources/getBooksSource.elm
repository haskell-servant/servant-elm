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

getBooks : Bool -> (Maybe String) -> (Maybe Int) -> String -> (List (Maybe Bool)) -> (Result Http.Error  ((List Book))  -> msg) -> Cmd msg
getBooks query_published query_sort query_year query_category query_filters toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [ [ if query_published then
                    Just (Url.Builder.string "published" "")
                  else
                    Nothing ]
                , [ query_sort
                    |> Maybe.map (Url.Builder.string "sort") ]
                , [ query_year
                    |> Maybe.map (String.fromInt >> Url.Builder.string "year") ]
                , [ Just query_category
                    |> Maybe.map (Url.Builder.string "category") ]
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
                Url.Builder.crossOrigin ""
                    [ "books"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecBook))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
