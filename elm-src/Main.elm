module Main exposing (main)

import Browser as Browser
import Browser.Navigation as Nav
import Generated.API as API exposing (..)
import Html exposing (..)
import Html.Attributes exposing (checked, class, href, id, style, type_)
import Html.Events exposing (onCheck)
import Http
import Pages.Board as Board
import Pages.Graph as Graph
import Pages.Team as Team
import Score exposing (Score)
import Time exposing (Posix)
import Url
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, top)


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
    | Graph Graph.Model
    | Team Team.Model


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | CheckReload Bool
    | Tick Posix
    | FetchScores (Result Http.Error (List API.Score))
    | GraphMsg Graph.Msg
    | TeamMsg Team.Msg


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
stepUrl url model =
    let
        parser =
            oneOf
                [ route top
                    ( { model | page = Home }, API.getApiScores FetchScores )
                , route (s "graph")
                    ( { model | page = Graph (Graph.init model) }, API.getApiScores FetchScores )
                , route (s "teams" </> Parser.string)
                    (\id -> ( { model | page = Team (Team.init model id) }, API.getApiScores FetchScores ))
                ]
    in
    case Parser.parse parser url of
        Just answer ->
            answer

        Nothing ->
            ( model, Cmd.none )


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser


stepPageWith : ( model -> Page, msg -> Msg ) -> Model -> ( model, Cmd msg ) -> ( Model, Cmd Msg )
stepPageWith ( toPage, toMsg ) model ( local, msg ) =
    ( { model | page = toPage local }, Cmd.map toMsg msg )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        LinkClicked (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            stepUrl url model

        CheckReload reload ->
            ( { model | reload = reload }, Cmd.none )

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

        GraphMsg msg ->
            case model.page of
                Graph local ->
                    stepPageWith ( Graph, GraphMsg ) model (Graph.update msg local)

                _ ->
                    ( model, Cmd.none )

        TeamMsg msg ->
            case model.page of
                Team local ->
                    stepPageWith ( Team, TeamMsg ) model (Team.update msg local)

                _ ->
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
        [ div [ class "Header" ]
            [ div [ class "Header-item Header-item--full" ]
                [ a [ class "Header-link", href "/" ]
                    [ h2 [] [ text "Git Challenge ScoreBoard" ] ]
                ]
            , div [ class "Header-item" ]
                [ a [ class "Header-link", href "/graph" ]
                    [ text "Graph" ]
                ]
            , div [ class "Header-item mr-0" ] [ viewCheckReload model ]
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

        Graph local ->
            Html.map GraphMsg (Graph.view model local)

        Team local ->
            Html.map TeamMsg (Team.view model local)
