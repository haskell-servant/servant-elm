module Generated.Api (..) where
import Http exposing (Error, empty, fromJson, send, defaultSettings, uriEncode, string)
import Json.Decode
import String
import Task exposing (Task)

postCounterInc : Json.Decode.Decoder a  -> Task Error a
postCounterInc decoder  =
  let request =
        { verb = "POST"
        , headers =
            [("Content-Type", "application/json")]
        , url = "http://localhost:8080/counter/inc"
        , body = empty
        }
  in
      fromJson decoder (send defaultSettings request)


getCounter : Json.Decode.Decoder a  -> Task Error a
getCounter decoder  =
  let request =
        { verb = "GET"
        , headers =
            [("Content-Type", "application/json")]
        , url = "http://localhost:8080/counter"
        , body = empty
        }
  in
      fromJson decoder (send defaultSettings request)

