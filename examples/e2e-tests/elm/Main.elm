module Main exposing (..)

import Generated.Api as Api
import Html exposing (div, img, input, button, text, li, ul, h1, dl, dd, dt)
import Http
import Browser

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { successGetIp : Maybe (Result Http.Error Api.Response)
    , successGetStatus204 : Maybe (Result Http.Error Api.NoContent)
    , successPostPost : Maybe (Result Http.Error Api.ResponseWithJson)
    , successGetGet : Maybe (Result MyError Api.ResponseWithArgs)
    , successGetByPath : Maybe (Result Http.Error Api.Response)
    }


type MyError
    = HttpError Http.Error
    | AppError String


init : () -> ( Model, Cmd Msg )
init _ =
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
    = SetSuccessStatus204 (Result Http.Error Api.NoContent)
    | SetSuccessIp (Result Http.Error Api.Response)
    | SetSuccessPost (Result Http.Error Api.ResponseWithJson)
    | CheckSuccessGet (Result Http.Error Api.ResponseWithArgs)
    | SetSuccessGetByPath (Result Http.Error Api.Response)


fetchStatus204 : Cmd Msg
fetchStatus204 =
    Api.getStatus204
        |> Http.send SetSuccessStatus204


fetchIp : Cmd Msg
fetchIp =
    Api.getIp
        |> Http.send SetSuccessIp


postPost : Cmd Msg
postPost =
    Api.postPost { message = "Hello World" }
        |> Http.send SetSuccessPost


getGet : Cmd Msg
getGet =
    Api.getGet (Just "Hello World")
        |> Http.send CheckSuccessGet


getByPath : Cmd Msg
getByPath =
    Api.getByPath "get"
        |> Http.send SetSuccessGetByPath


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
                                    Err (AppError (response.args.q ++ " != " ++ "Hello World"))

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


promoteError : Result Http.Error a -> Result MyError a
promoteError result =
    case result of
        Ok a ->
            Ok a

        Err e ->
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
                    ( ": Error", "")

                Just (Ok x) ->
                    ( ": Ok", "")
    in
        [ dt [] [ text (name ++ status) ]
        , dd [] [ text content ]
        ]
