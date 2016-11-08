module Generated.GiphyApi exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String
import Task


type alias Gif =
    { data :GifData
    }

type alias GifData =
    { image_url : String
    }

decodeGif : Decoder Gif
decodeGif =
    decode Gif
        |> required "data" decodeGifData

decodeGifData : Decoder GifData
decodeGifData =
    decode GifData
        |> required "image_url" string

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