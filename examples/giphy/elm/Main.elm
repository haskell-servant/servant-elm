module Main exposing (..)

import Generated.GiphyApi as Api

import Html.App as Html
import Html exposing (div, img, input, button, text)
import Html.Attributes exposing (placeholder, src, value)
import Html.Events exposing (onClick, onInput, targetValue)
import String
import Task

main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }

type alias Model =
  { url : Maybe String
  , topic : Maybe String
  }

init : (Model, Cmd Msg)
init =
  ( { url = Nothing
    , topic = Nothing
    }
  , Cmd.none)

type Msg
  = FetchGif
  | NewGif (Maybe Api.Gif)
  | SetTopic String

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    FetchGif ->
      let
        effects =
          Api.getRandom (Just "dc6zaTOxFJmzC") model.topic
             |> Task.perform (always (NewGif Nothing)) (NewGif << Just)
      in
        ( { model
              | url = Nothing
          }
        , effects)

    NewGif mGif ->
      ( { model
            | url =
                Maybe.map (.data >> .image_url) mGif
        }
      , Cmd.none)

    SetTopic topic ->
      ( { model
            | topic =
                if String.isEmpty topic then
                  Nothing
                else
                  Just topic
        }
      , Cmd.none)

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
