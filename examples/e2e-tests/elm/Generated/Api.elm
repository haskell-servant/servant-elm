module Generated.Api where

import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Http
import String
import Task


type alias Response =
  { origin : String
  }

decodeResponse : Json.Decode.Decoder Response
decodeResponse =
  Json.Decode.succeed Response
    |: ("origin" := Json.Decode.string)

encodeResponse : Response -> Json.Encode.Value
encodeResponse x =
  Json.Encode.object
    [ ( "origin", Json.Encode.string x.origin )
    ]

getIp : Task.Task Http.Error (Response)
getIp =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "https://httpbin.org"
          ++ "/" ++ "ip"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeResponse
      (Http.send Http.defaultSettings request)

type NoContent
  = NoContent

emptyResponseHandler : a -> String -> Task.Task Http.Error a
emptyResponseHandler x str =
  if String.isEmpty str then
    Task.succeed x
  else
    Task.fail (Http.UnexpectedPayload str)

handleResponse : (String -> Task.Task Http.Error a) -> Http.Response -> Task.Task Http.Error a
handleResponse handle response =
  if 200 <= response.status && response.status < 300 then
    case response.value of
      Http.Text str ->
        handle str
      _ ->
        Task.fail (Http.UnexpectedPayload "Response body is a blob, expecting a string.")
  else
    Task.fail (Http.BadResponse response.status response.statusText)

promoteError : Http.RawError -> Http.Error
promoteError rawError =
  case rawError of
    Http.RawTimeout -> Http.Timeout
    Http.RawNetworkError -> Http.NetworkError

getStatus204 : Task.Task Http.Error (NoContent)
getStatus204 =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "https://httpbin.org"
          ++ "/" ++ "status"
          ++ "/" ++ "204"
      , body =
          Http.empty
      }
  in
    Task.mapError promoteError
      (Http.send Http.defaultSettings request)
        `Task.andThen`
          handleResponse (emptyResponseHandler NoContent)

type alias MessageBody =
  { message : String
  }

type alias ResponseWithJson =
  { json : MessageBody
  }

decodeMessageBody : Json.Decode.Decoder MessageBody
decodeMessageBody =
  Json.Decode.succeed MessageBody
    |: ("message" := Json.Decode.string)

decodeResponseWithJson : Json.Decode.Decoder ResponseWithJson
decodeResponseWithJson =
  Json.Decode.succeed ResponseWithJson
    |: ("json" := decodeMessageBody)

encodeMessageBody : MessageBody -> Json.Encode.Value
encodeMessageBody x =
  Json.Encode.object
    [ ( "message", Json.Encode.string x.message )
    ]

encodeResponseWithJson : ResponseWithJson -> Json.Encode.Value
encodeResponseWithJson x =
  Json.Encode.object
    [ ( "json", encodeMessageBody x.json )
    ]

postPost : MessageBody -> Task.Task Http.Error (ResponseWithJson)
postPost body =
  let
    request =
      { verb =
          "POST"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "https://httpbin.org"
          ++ "/" ++ "post"
      , body =
          Http.string (Json.Encode.encode 0 (encodeMessageBody body))
      }
  in
    Http.fromJson
      decodeResponseWithJson
      (Http.send Http.defaultSettings request)

type alias ResponseWithArgs =
  { args : QueryArgs
  }

type alias QueryArgs =
  { q : String
  }

decodeResponseWithArgs : Json.Decode.Decoder ResponseWithArgs
decodeResponseWithArgs =
  Json.Decode.succeed ResponseWithArgs
    |: ("args" := decodeQueryArgs)

decodeQueryArgs : Json.Decode.Decoder QueryArgs
decodeQueryArgs =
  Json.Decode.succeed QueryArgs
    |: ("q" := Json.Decode.string)

encodeResponseWithArgs : ResponseWithArgs -> Json.Encode.Value
encodeResponseWithArgs x =
  Json.Encode.object
    [ ( "args", encodeQueryArgs x.args )
    ]

encodeQueryArgs : QueryArgs -> Json.Encode.Value
encodeQueryArgs x =
  Json.Encode.object
    [ ( "q", Json.Encode.string x.q )
    ]

getGet : Maybe (String) -> Task.Task Http.Error (ResponseWithArgs)
getGet q =
  let
    params =
      List.filter (not << String.isEmpty)
        [ q
            |> Maybe.map (toString >> Http.uriEncode >> (++) "q=")
            |> Maybe.withDefault ""
        ]
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "https://httpbin.org"
          ++ "/" ++ "get"
          ++ if List.isEmpty params then
               ""
             else
               "?" ++ String.join "&" params
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeResponseWithArgs
      (Http.send Http.defaultSettings request)

getByPath : String -> Task.Task Http.Error (Response)
getByPath path =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "https://httpbin.org"
          ++ "/" ++ (path |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeResponse
      (Http.send Http.defaultSettings request)