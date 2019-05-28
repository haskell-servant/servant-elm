module Generated.MyApi exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String
import Url.Builder

maybeBoolToIntStr : Maybe Bool -> String
maybeBoolToIntStr mx =
  case mx of
    Nothing -> ""
    Just True -> "1"
    Just False -> "0"

type alias Book  =
   { name: String
   }

jsonDecBook : Json.Decode.Decoder ( Book )
jsonDecBook =
   Json.Decode.succeed (\pname -> {name = pname})
   |> required "name" (Json.Decode.string)

jsonEncBook : Book -> Value
jsonEncBook  val =
   Json.Encode.object
   [ ("name", Json.Encode.string val.name)
   ]


getBooksByBookId : Int -> Http.Request Book
getBooksByBookId capture_bookId =
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
                Url.Builder.absolute
                    [ "books"
                    , capture_bookId |> String.fromInt
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson <| jsonDecBook
            , timeout =
                Nothing
            , withCredentials =
                False
            }
