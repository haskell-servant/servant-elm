module Generated.GiphyApi exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String.Conversions as String
import Url


type alias Gif =
    { data : GifData
    }

type alias GifData =
    { image_url : String
    }

decodeGif : Decoder Gif
decodeGif =
    succeed Gif
        |> required "data" decodeGifData

decodeGifData : Decoder GifData
decodeGifData =
    succeed GifData
        |> required "image_url" string

getRandom : Maybe (String) -> Maybe (String) -> Http.Request (Http.Response (Gif))
getRandom query_api_key query_tag =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_api_key
                    |> Maybe.map (Url.percentEncode >> (++) "api_key=")
                    |> Maybe.withDefault ""
                , query_tag
                    |> Maybe.map (Url.percentEncode >> (++) "tag=")
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
                Http.expectStringResponse
                    (\res ->
                        Result.mapError Json.Decode.errorToString
                            (Result.map
                                (\body_ -> { url = res.url, status = res.status, headers = res.headers, body = body_ })
                                (decodeString decodeGif res.body)))
            , timeout =
                Nothing
            , withCredentials =
                False
            }
