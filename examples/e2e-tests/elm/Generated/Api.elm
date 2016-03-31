module Generated.Api where

import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Http
import String
import Task


type alias OriginIp =
  { origin : String
  }

decodeOriginIp : Json.Decode.Decoder OriginIp
decodeOriginIp =
  Json.Decode.succeed OriginIp
    |: ("origin" := Json.Decode.string)

encodeOriginIp : OriginIp -> Json.Encode.Value
encodeOriginIp x =
  Json.Encode.object
    [ ( "origin", Json.Encode.string x.origin )
    ]

getIp : Task.Task Http.Error (OriginIp)
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
      decodeOriginIp
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

type alias MessageResponse =
  { json : MessageBody
  }

decodeMessageBody : Json.Decode.Decoder MessageBody
decodeMessageBody =
  Json.Decode.succeed MessageBody
    |: ("message" := Json.Decode.string)

decodeMessageResponse : Json.Decode.Decoder MessageResponse
decodeMessageResponse =
  Json.Decode.succeed MessageResponse
    |: ("json" := decodeMessageBody)

encodeMessageBody : MessageBody -> Json.Encode.Value
encodeMessageBody x =
  Json.Encode.object
    [ ( "message", Json.Encode.string x.message )
    ]

encodeMessageResponse : MessageResponse -> Json.Encode.Value
encodeMessageResponse x =
  Json.Encode.object
    [ ( "json", encodeMessageBody x.json )
    ]

postPost : MessageBody -> Task.Task Http.Error (MessageResponse)
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
      decodeMessageResponse
      (Http.send Http.defaultSettings request)