module Generated.GiphyApi where

import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Http
import String
import Task


type alias Gif =
  { _data : GifData
  }

type alias GifData =
  { image_url : String
  }

decodeGif : Json.Decode.Decoder Gif
decodeGif =
  Json.Decode.succeed Gif
    |: ("_data" := decodeGifData)

decodeGifData : Json.Decode.Decoder GifData
decodeGifData =
  Json.Decode.succeed GifData
    |: ("image_url" := Json.Decode.string)

encodeGif : Gif -> Json.Encode.Value
encodeGif x =
  Json.Encode.object
    [ ( "_data", encodeGifData x._data )
    ]

encodeGifData : GifData -> Json.Encode.Value
encodeGifData x =
  Json.Encode.object
    [ ( "image_url", Json.Encode.string x.image_url )
    ]

getRandom : Maybe (String) -> Maybe (String) -> Task.Task Http.Error (Gif)
getRandom api_key tag =
  let
    params =
      List.filter (not << String.isEmpty)
        [ api_key
            |> Maybe.map (Http.uriEncode >> (++) "api_key=")
            |> Maybe.withDefault ""
        , tag
            |> Maybe.map (Http.uriEncode >> (++) "tag=")
            |> Maybe.withDefault ""
        ]
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "http://api.giphy.com/v1/gifs"
          ++ "/" ++ "random"
          ++ if List.isEmpty params then
               ""
             else
               "?" ++ String.join "&" params
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeGif
      (Http.send Http.defaultSettings request)