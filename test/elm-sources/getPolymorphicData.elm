module GetPolymorphicData exposing (..)

import Http
import Json.Decode exposing (..)
import Url.Builder

type PolymorphicData a b = PolymorphicData a b
type SomeRecord = SomeRecord { recordId : Int, recordname : String }

jsonDecPolymorphicData : Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder (PolymorphicData a b)
jsonDecPolymorphicData _ _ = Debug.todo "finish"

jsonDecSomeRecord : Json.Decode.Decoder SomeRecord
jsonDecSomeRecord = Debug.todo "finish"


getPolymorphicData : (Result Http.Error  ((PolymorphicData (List String) SomeRecord))  -> msg) -> Cmd msg
getPolymorphicData toMsg =
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
                Url.Builder.crossOrigin ""
                    [ "polymorphicData"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg ((jsonDecPolymorphicData (Json.Decode.list (Json.Decode.string))) jsonDecSomeRecord)
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
