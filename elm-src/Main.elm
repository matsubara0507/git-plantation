module Main exposing (Model, Msg(..), init, main, subscriptions, update, view, viewCheckReload)

import Browser as Browser
import Generated.API as API exposing (..)
import Html exposing (..)
import Html.Attributes exposing (checked, class, type_)
import Html.Events exposing (onCheck, onClick)
import RemoteData exposing (RemoteData(..))
import Time exposing (Posix)
import Generated.API as API
import Http

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { reload : Bool
    , problems : RemoteData String (List API.Problem)
    , teams : RemoteData String (List API.Team)
    , scores : RemoteData String (List API.Score)
    }


type Msg
    = CheckReload Bool
    | Reload
    | Tick Posix
    | FetchProblems (Result Http.Error (List API.Problem))
    | FetchTeams (Result Http.Error (List API.Team))
    | FetchScores (Result Http.Error (List API.Score))


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { reload = True
            , problems = NotAsked
            , teams = NotAsked
            , scores = NotAsked
            }
    in
    ( model, Cmd.batch [ fetchProblems, fetchTeams, fetchScores ] )


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
            , viewScores model
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


viewScores : Model -> Html Msg
viewScores model =
    text (Debug.toString model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CheckReload reload ->
            ( { model | reload = reload }, Cmd.none )

        Reload ->
            ( model, fetchScores )

        Tick _ ->
            ( model
            , if model.reload then
                fetchScores

              else
                Cmd.none
            )

        FetchProblems (Ok problems) ->
            ( { model | problems = Success problems }, Cmd.none )

        FetchProblems (Err _) ->
            ( { model | problems = Failure "Something went wrong.." }, Cmd.none )

        FetchTeams (Ok teams) ->
            ( { model | teams = Success teams }, Cmd.none )

        FetchTeams (Err _) ->
            ( { model | teams = Failure "Something went wrong.." }, Cmd.none )

        FetchScores (Ok scores) ->
            ( { model | scores = Success scores }, Cmd.none )

        FetchScores (Err _) ->
            ( { model | scores = Failure "Something went wrong.." }, Cmd.none )


fetchProblems : Cmd Msg
fetchProblems =
    Http.send FetchProblems API.getApiProblems


fetchTeams : Cmd Msg
fetchTeams =
    Http.send FetchTeams API.getApiTeams


fetchScores : Cmd Msg
fetchScores =
    Http.send FetchScores API.getApiScores


baseUrl : String
baseUrl =
    "localhost:8080"


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 60000 Tick
