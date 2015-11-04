module Main where

import Generated.Api as Api

import Effects exposing (Effects)
import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json
import StartApp
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

type alias Counter = Int

type alias Model =
  { counter : Counter }


init : (Model, Effects Action)
init = fetchCounter { counter = 0 }


type Action
  = IncCounter
  | FetchCounter
  | SetCounter (Maybe Counter)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    FetchCounter ->
      fetchCounter model

    IncCounter ->
      incCounter model

    SetCounter (Just counter) ->
      pure { model | counter <- counter }

    SetCounter Nothing ->
      pure model


view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ h1 [] [ text "Counters" ]
    , div []
        [ text ("Count is " ++ (toString model.counter))
        , button [onClick address IncCounter] [text "++"]
        ]
    ]


decodeCounter : Json.Decoder Counter
decodeCounter = Json.int


fetchCounter : Model -> (Model, Effects Action)
fetchCounter model =
  let task = Api.getCounter decodeCounter
      effect = toSetCounterEffect task
  in (model, effect)


incCounter : Model -> (Model, Effects Action)
incCounter model =
  let task = Api.postCounterInc decodeCounter
      effect = toSetCounterEffect task
  in ({ model | counter <- model.counter + 1 }, effect)


toSetCounterEffect : Task.Task Http.Error Counter -> Effects Action
toSetCounterEffect task =
  task
    |> Task.toMaybe
    |> Task.map SetCounter
    |> Effects.task


pure : a -> (a, Effects b)
pure model = (model, Effects.none)
