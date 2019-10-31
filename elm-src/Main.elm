module Main exposing (main)

import Browser as Browser
import Browser.Navigation as Nav
import Generated.API as API exposing (..)
import Html exposing (..)
import Html.Attributes exposing (checked, class, href, id, style, target, type_)
import Html.Events exposing (onCheck, onClick)
import Http
import Pages.Board as Board
import Score exposing (Score)
import Time exposing (Posix)
import Url


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type alias Model =
    { key : Nav.Key
    , page : Page
    , config : API.ScoreBoardConfig
    , problems : List API.Problem
    , scores : List Score
    , reload : Bool
    }


type Page
    = Home -- Board


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | CheckReload Bool
    | Reload
    | Tick Posix
    | FetchScores (Result Http.Error (List API.Score))


init : { config : API.Config } -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init { config } url key =
    stepUrl url
        { key = key
        , page = Home
        , config = config.scoreboard
        , reload = True
        , problems = config.problems
        , scores = Score.build config []
        }


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl _ model =
    ( model, Cmd.batch [ API.getApiScores FetchScores ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            stepUrl url model

        CheckReload reload ->
            ( { model | reload = reload }, Cmd.none )

        Reload ->
            ( model, API.getApiScores FetchScores )

        Tick _ ->
            ( model
            , if model.reload then
                API.getApiScores FetchScores

              else
                Cmd.none
            )

        FetchScores (Ok resp) ->
            ( { model | scores = Score.updateBy resp model.scores }, Cmd.none )

        FetchScores (Err _) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every model.config.interval Tick


view : Model -> Browser.Document Msg
view model =
    { title = "Git Challenge ScoreBoard"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ div [ class "my-3 mx-auto col-10" ]
        [ div []
            [ h2
                [ class "f1-light float-left link-gray-dark"
                , onClick Reload
                ]
                [ text "Git Challenge ScoreBoard" ]
            , div [ class "float-right" ] [ viewCheckReload model ]
            ]
        , viewPage model
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


viewPage : Model -> Html Msg
viewPage model =
    case model.page of
        Home ->
            Board.view model
