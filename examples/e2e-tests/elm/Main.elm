module Main (..) where

import Generated.Api as Api
import Effects
import Html exposing (div, img, input, button, text, li, ul, h1, dl, dd, dt)
import Html.Attributes exposing (placeholder, src, value)
import Html.Events exposing (onClick, on, targetValue)
import Http
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
  { successGetIp : Maybe (Result Http.Error Api.OriginIp)
  , successGetStatus204 : Maybe (Result Http.Error Api.NoContent)
  , successPostPost : Maybe (Result Http.Error Api.MessageResponse)
  }


init : ( Model, Effects.Effects Action )
init =
  ( { successGetIp = Nothing
    , successGetStatus204 = Nothing
    , successPostPost = Nothing
    }
  , Effects.batch
      [ fetchStatus204
      , fetchIp
      , postPost
      ]
  )


type Action
  = SetSuccessStatus204 (Result Http.Error Api.NoContent)
  | SetSuccessIp (Result Http.Error Api.OriginIp)
  | SetSuccessPost (Result Http.Error Api.MessageResponse)


fetchStatus204 : Effects.Effects Action
fetchStatus204 =
  Api.getStatus204
    |> Task.toResult
    |> Task.map SetSuccessStatus204
    |> Effects.task


fetchIp : Effects.Effects Action
fetchIp =
  Api.getIp
    |> Task.toResult
    |> Task.map SetSuccessIp
    |> Effects.task


postPost : Effects.Effects Action
postPost =
  Api.postPost { message = "Hello World" }
    |> Task.toResult
    |> Task.map SetSuccessPost
    |> Effects.task


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action of
    SetSuccessStatus204 result ->
      ( { model | successGetStatus204 = Just result }
      , Effects.none
      )

    SetSuccessIp result ->
      ( { model | successGetIp = Just result }
      , Effects.none
      )

    SetSuccessPost result ->
      ( { model | successPostPost = Just result }
      , Effects.none
      )


view : Signal.Address Action -> Model -> Html.Html
view address model =
  div
    []
    [ h1 [] [ text "Tests" ]
    , dl
        []
        (List.concat
          [ viewResult "getIp" model.successGetIp
          , viewResult "getStatus204" model.successGetStatus204
          , viewResult "postPost" model.successPostPost
          ]
        )
    ]


viewResult : String -> Maybe (Result Http.Error a) -> List Html.Html
viewResult name success =
  let
    ( status, content ) =
      case success of
        Nothing ->
          ( ": Waiting...", "" )

        Just (Err e) ->
          ( ": Error", toString e )

        Just (Ok x) ->
          ( ": Ok", toString x )
  in
    [ dt [] [ text (name ++ status) ]
    , dd [] [ text content ]
    ]
