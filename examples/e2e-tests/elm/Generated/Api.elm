module Generated.Api exposing(..)

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

type alias MessageBody  =
   { message: String
   }

jsonDecMessageBody : Json.Decode.Decoder ( MessageBody )
jsonDecMessageBody =
   Json.Decode.succeed (\pmessage -> {message = pmessage})
   |> required "message" (Json.Decode.string)

jsonEncMessageBody : MessageBody -> Value
jsonEncMessageBody  val =
   Json.Encode.object
   [ ("message", Json.Encode.string val.message)
   ]



type alias QueryArgs  =
   { q: String
   }

jsonDecQueryArgs : Json.Decode.Decoder ( QueryArgs )
jsonDecQueryArgs =
   Json.Decode.succeed (\pq -> {q = pq})
   |> required "q" (Json.Decode.string)

jsonEncQueryArgs : QueryArgs -> Value
jsonEncQueryArgs  val =
   Json.Encode.object
   [ ("q", Json.Encode.string val.q)
   ]



type alias Response  =
   { origin: String
   }

jsonDecResponse : Json.Decode.Decoder ( Response )
jsonDecResponse =
   Json.Decode.succeed (\porigin -> {origin = porigin})
   |> required "origin" (Json.Decode.string)

jsonEncResponse : Response -> Value
jsonEncResponse  val =
   Json.Encode.object
   [ ("origin", Json.Encode.string val.origin)
   ]



type alias ResponseWithJson  =
   { json: MessageBody
   }

jsonDecResponseWithJson : Json.Decode.Decoder ( ResponseWithJson )
jsonDecResponseWithJson =
   Json.Decode.succeed (\pjson -> {json = pjson})
   |> required "json" (jsonDecMessageBody)

jsonEncResponseWithJson : ResponseWithJson -> Value
jsonEncResponseWithJson  val =
   Json.Encode.object
   [ ("json", jsonEncMessageBody val.json)
   ]



type alias ResponseWithArgs  =
   { args: QueryArgs
   }

jsonDecResponseWithArgs : Json.Decode.Decoder ( ResponseWithArgs )
jsonDecResponseWithArgs =
   Json.Decode.succeed (\pargs -> {args = pargs})
   |> required "args" (jsonDecQueryArgs)

jsonEncResponseWithArgs : ResponseWithArgs -> Value
jsonEncResponseWithArgs  val =
   Json.Encode.object
   [ ("args", jsonEncQueryArgs val.args)
   ]


getIp : Http.Request Response
getIp =
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
                    [ "ip"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson <| jsonDecResponse
            , timeout =
                Nothing
            , withCredentials =
                False
            }

getStatus204 : Http.Request NoContent
getStatus204 =
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
                    [ "status"
                    , "204"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectStringResponse
                    (\ rsp  ->
                        if String.isEmpty rsp.body then
                            Ok NoContent
                        else
                            Err "Expected the response body to be empty"
                    )
            , timeout =
                Nothing
            , withCredentials =
                False
            }

postPost : MessageBody -> Http.Request ResponseWithJson
postPost body =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.absolute
                    [ "post"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncMessageBody body)
            , expect =
                Http.expectJson <| jsonDecResponseWithJson
            , timeout =
                Nothing
            , withCredentials =
                False
            }

getGet : (Maybe String) -> Http.Request ResponseWithArgs
getGet query_q =
    let
        params =
            List.filterMap identity
            (List.concat
                [ [ query_q
                    |> Maybe.map (Url.Builder.string "query_q") ]
                ])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.absolute
                    [ "get"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson <| jsonDecResponseWithArgs
            , timeout =
                Nothing
            , withCredentials =
                False
            }

getByPath : String -> Http.Request Response
getByPath capture_path =
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
                    [ capture_path
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson <| jsonDecResponse
            , timeout =
                Nothing
            , withCredentials =
                False
            }
