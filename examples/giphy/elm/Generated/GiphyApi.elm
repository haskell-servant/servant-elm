module Generated.GiphyApi exposing(..)

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

type alias Gif  =
   { data: GifData
   }

jsonDecGif : Json.Decode.Decoder ( Gif )
jsonDecGif =
   Json.Decode.succeed (\pdata -> {data = pdata})
   |> required "data" (jsonDecGifData)

jsonEncGif : Gif -> Value
jsonEncGif  val =
   Json.Encode.object
   [ ("data", jsonEncGifData val.data)
   ]



type alias GifData  =
   { image_url: String
   }

jsonDecGifData : Json.Decode.Decoder ( GifData )
jsonDecGifData =
   Json.Decode.succeed (\pimage_url -> {image_url = pimage_url})
   |> required "image_url" (Json.Decode.string)

jsonEncGifData : GifData -> Value
jsonEncGifData  val =
   Json.Encode.object
   [ ("image_url", Json.Encode.string val.image_url)
   ]


getRandom : (Maybe String) -> (Maybe String) -> Http.Request Gif
getRandom query_api_key query_tag =
    let
        params =
            List.filterMap identity
            (List.concat
                [ [ query_api_key
                    |> Maybe.map (Url.Builder.string "query_api_key") ]
                , [ query_tag
                    |> Maybe.map (Url.Builder.string "query_tag") ]
                ])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.absolute
                    [ "random"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson <| jsonDecGif
            , timeout =
                Nothing
            , withCredentials =
                False
            }
