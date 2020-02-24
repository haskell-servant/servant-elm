module Main exposing (..)

import Generated.Api as Api
import Html exposing (div, img, input, button, text, li, ul, h1, dl, dd, dt)
import Http
import Browser
import Debug


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { successGetIp : Maybe (Result ( Maybe ( Http.Metadata, String ), Http.Error ) Api.Response)
    , successGetStatus204 : Maybe (Result ( Maybe ( Http.Metadata, String ), Http.Error ) Api.NoContent)
    , successPostPost : Maybe (Result ( Maybe ( Http.Metadata, String ), Http.Error ) Api.ResponseWithJson)
    , successGetGet : Maybe (Result MyError Api.ResponseWithArgs)
    , successGetByPath : Maybe (Result ( Maybe ( Http.Metadata, String ), Http.Error ) Api.Response)
    }


type MyError
    = HttpError Http.Error
    | AppError String


init : ( Model, Cmd Msg )
init =
    ( { successGetIp = Nothing
      , successGetStatus204 = Nothing
      , successPostPost = Nothing
      , successGetGet = Nothing
      , successGetByPath = Nothing
      }
    , Cmd.batch
        [ fetchStatus204
        , fetchIp
        , postPost
        , getGet
        , getByPath
        ]
    )


type Msg
    = SetSuccessStatus204 (Result ( Maybe ( Http.Metadata, String ), Http.Error ) Api.NoContent)
    | SetSuccessIp (Result ( Maybe ( Http.Metadata, String ), Http.Error ) Api.Response)
    | SetSuccessPost (Result ( Maybe ( Http.Metadata, String ), Http.Error ) Api.ResponseWithJson)
    | CheckSuccessGet (Result ( Maybe ( Http.Metadata, String ), Http.Error ) Api.ResponseWithArgs)
    | SetSuccessGetByPath (Result ( Maybe ( Http.Metadata, String ), Http.Error ) Api.Response)


fetchStatus204 : Cmd Msg
fetchStatus204 =
    Api.getStatus204 SetSuccessStatus204


fetchIp : Cmd Msg
fetchIp =
    Api.getIp SetSuccessIp


postPost : Cmd Msg
postPost =
    Api.postPost SetSuccessPost { message = "Hello World" }


getGet : Cmd Msg
getGet =
    Api.getGet CheckSuccessGet (Just "Hello World")


getByPath : Cmd Msg
getByPath =
    Api.getByPath SetSuccessGetByPath "get"


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        SetSuccessStatus204 result ->
            ( { model | successGetStatus204 = Just result }
            , Cmd.none
            )

        SetSuccessIp result ->
            ( { model | successGetIp = Just result }
            , Cmd.none
            )

        SetSuccessPost result ->
            ( { model | successPostPost = Just result }
            , Cmd.none
            )

        CheckSuccessGet result ->
            ( { model
                | successGetGet =
                    Just
                        (case result of
                            Ok response ->
                                if response.args.q == "Hello World" then
                                    promoteError result
                                else
                                    Err (AppError (Debug.toString response.args.q ++ " != " ++ Debug.toString "Hello World"))

                            Err _ ->
                                promoteError result
                        )
              }
            , Cmd.none
            )

        SetSuccessGetByPath result ->
            ( { model | successGetByPath = Just result }
            , Cmd.none
            )


promoteError : Result ( Maybe ( Http.Metadata, String ), Http.Error ) a -> Result MyError a
promoteError result =
    case result of
        Ok a ->
            Ok a

        Err (_, e) ->
            Err (HttpError e)


view : Model -> Html.Html msg
view model =
    div
        []
        [ h1 [] [ text "Tests" ]
        , dl
            []
            (List.concat
                [ viewResult "getIp" model.successGetIp
                , viewResult "getStatus204" model.successGetStatus204
                , viewResult "postPost" model.successPostPost
                , viewResult "getGet" model.successGetGet
                , viewResult "getByPath" model.successGetByPath
                ]
            )
        ]


viewResult : String -> Maybe (Result e a) -> List (Html.Html msg)
viewResult name success =
    let
        ( status, content ) =
            case success of
                Nothing ->
                    ( ": Waiting...", "" )

                Just (Err e) ->
                    ( ": Error", Debug.toString e )

                Just (Ok x) ->
                    ( ": Ok", Debug.toString x )
    in
        [ dt [] [ text (name ++ status) ]
        , dd [] [ text content ]
        ]
