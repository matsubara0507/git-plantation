module Main exposing (Model, Msg(..), init, main, subscriptions, update, view, viewCheckReload)

import Browser as Browser
import Html exposing (..)
import Html.Attributes exposing (checked, class, type_)
import Html.Events exposing (onCheck, onClick)
import Time exposing (Posix)
import Generated.API as API

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { reload : Bool
    }


type Msg
    = CheckReload Bool
    | Reload
    | Tick Posix


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { reload = True
            }
    in
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "" ]
        [ div [ class "my-3 mx-auto col-10 col-lg-8" ]
            [ div []
                [ h2
                    [ class "f1-light float-left link-gray-dark"
                    , onClick Reload
                    ]
                    [ text "Git Challenge ScoreBoard" ]
                , div [ class "float-right" ] [ viewCheckReload model ]
                ]

            -- , viewScores model
            ]
        ]


viewCheckReload : Model -> Html Msg
viewCheckReload model =
    form []
        [ div [ class "form-checkbox" ]
            [ label []
                [ input
                    [ type_ "checkbox"
                    , checked model.reload
                    , onCheck CheckReload
                    ]
                    []
                , text "Auto Reload"
                ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CheckReload reload ->
            ( { model | reload = reload }, Cmd.none )

        Reload ->
            ( model, Cmd.none )

        Tick _ ->
            ( model
            , if model.reload then
                Cmd.none

              else
                Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 60000 Tick
