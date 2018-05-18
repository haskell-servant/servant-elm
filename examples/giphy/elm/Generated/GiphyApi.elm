module Generated.GiphyApi exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String

type alias Gif  =
   { data: GifData
   }

jsonDecGif : Json.Decode.Decoder ( Gif )
jsonDecGif =
   ("data" := jsonDecGifData) >>= \pdata ->
   Json.Decode.succeed {data = pdata}

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
   ("image_url" := Json.Decode.string) >>= \pimage_url ->
   Json.Decode.succeed {image_url = pimage_url}

jsonEncGifData : GifData -> Value
jsonEncGifData  val =
   Json.Encode.object
   [ ("image_url", Json.Encode.string val.image_url)
   ]


getRandom : (Maybe String) -> (Maybe String) -> Http.Request Gif
getRandom query_api_key query_tag =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_api_key
                    |> Maybe.map (Http.encodeUri >> (++) "api_key=")
                    |> Maybe.withDefault ""
                , query_tag
                    |> Maybe.map (Http.encodeUri >> (++) "tag=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                String.join "/"
                    [ "http://api.giphy.com/v1/gifs"
                    , "random"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson <| jsonDecGif
            , timeout =
                Nothing
            , withCredentials =
                False
            }
