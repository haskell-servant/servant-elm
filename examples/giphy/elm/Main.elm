module Main exposing (..)

import Generated.GiphyApi as Api
import Browser
import Html exposing (div, img, input, button, text)
import Html.Attributes exposing (placeholder, src, value)
import Html.Events exposing (onClick, onInput, targetValue)
import Http
import String


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { url : Maybe String
    , topic : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { url = Nothing
      , topic = Nothing
      }
    , Cmd.none
    )


type Msg
    = FetchGif
    | NewGif (Result Http.Error Api.Gif)
    | SetTopic String


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        FetchGif ->
            let
                effects =
                    Api.getRandom (Just "dc6zaTOxFJmzC") model.topic
                        |> Http.send NewGif
            in
                ( { model
                    | url = Nothing
                  }
                , effects
                )

        NewGif rGif ->
            ( { model
                | url =
                    rGif
                    |> Result.toMaybe
                    |> Maybe.map (.data >> .image_url)
              }
            , Cmd.none
            )

        SetTopic topic ->
            ( { model
                | topic =
                    if String.isEmpty topic then
                        Nothing
                    else
                        Just topic
              }
            , Cmd.none
            )


view : Model -> Html.Html Msg
view model =
    div []
        [ div []
            [ input
                [ onInput SetTopic
                , value (Maybe.withDefault "" model.topic)
                , placeholder "topic"
                ]
                []
            , button
                [ onClick FetchGif ]
                [ text "click me" ]
            ]
        , img [ src (Maybe.withDefault "" model.url) ] []
        ]
