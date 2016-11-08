module Generated.Api exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String
import Task


type alias Response =
    { origin : String
    }

decodeResponse : Decoder Response
decodeResponse =
    decode Response
        |> required "origin" string

type NoContent
    = NoContent

type alias MessageBody =
    { message : String
    }

encodeMessageBody : MessageBody -> Json.Encode.Value
encodeMessageBody x =
    Json.Encode.object
        [ ( "message", Json.Encode.string x.message )
        ]

decodeMessageBody : Decoder MessageBody
decodeMessageBody =
    decode MessageBody
        |> required "message" string

type alias ResponseWithJson =
    { json :MessageBody
    }

decodeResponseWithJson : Decoder ResponseWithJson
decodeResponseWithJson =
    decode ResponseWithJson
        |> required "json" decodeMessageBody

type alias QueryArgs =
    { q : String
    }

decodeQueryArgs : Decoder QueryArgs
decodeQueryArgs =
    decode QueryArgs
        |> required "q" string

type alias ResponseWithArgs =
    { args :QueryArgs
    }

decodeResponseWithArgs : Decoder ResponseWithArgs
decodeResponseWithArgs =
    decode ResponseWithArgs
        |> required "args" decodeQueryArgs

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

getGet : Maybe (String) -> Task.Task Http.Error (ResponseWithArgs)
getGet q =
  let
    params =
      List.filter (not << String.isEmpty)
        [ q
            |> Maybe.map (Http.uriEncode >> (++) "q=")
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
          ++ "/" ++ (path |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeResponse
      (Http.send Http.defaultSettings request)