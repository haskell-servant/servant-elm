module Main where

import Generated.GiphyApi as Api

import Effects
import Html exposing (div, img, input, button, text)
import Html.Attributes exposing (placeholder, src, value)
import Html.Events exposing (onClick, on, targetValue)
import StartApp
import String
import Task

app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs = []
    }

main =
  app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks

type alias Model =
  { url : Maybe String
  , topic : Maybe String
  }

init : (Model, Effects.Effects Action)
init =
  ( { url = Nothing
    , topic = Nothing
    }
  , Effects.none)

type Action
  = FetchGif
  | NewGif (Maybe Api.Gif)
  | SetTopic String

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    FetchGif ->
      let
        effects =
          Api.getRandom (Just "dc6zaTOxFJmzC") model.topic
             |> Task.toMaybe
             |> Task.map NewGif
             |> Effects.task
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
      , Effects.none)

    SetTopic topic ->
      ( { model
            | topic =
                if String.isEmpty topic then
                  Nothing
                else
                  Just topic
        }
      , Effects.none)

view : Signal.Address Action -> Model -> Html.Html
view address model =
  div []
      [ div []
          [ input
              [ onInput address SetTopic
              , value (Maybe.withDefault "" model.topic)
              , placeholder "topic"
              ]
              []
          , button
              [ onClick address FetchGif ]
              [ text "click me" ]
          ]
      , img [ src (Maybe.withDefault "" model.url) ] []
      ]


onInput : Signal.Address a -> (String -> a) -> Html.Attribute
onInput address contentToValue =
  on "input" targetValue (\str -> Signal.message address (contentToValue str))
