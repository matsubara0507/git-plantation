module Main exposing (main)

import Browser as Browser
import Browser.Navigation as Nav
import Data exposing (Model, Msg(..))
import Generated.API as API exposing (..)
import Pages.Board as Board
import RemoteData exposing (RemoteData(..))
import Time
import Url exposing (Url)


type alias Flags =
    { config : API.Config }


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            { key = key
            , url = url
            , reload = True
            , problems = flags.config.problems
            , teams = flags.config.teams
            , scores = NotAsked
            , interval = flags.config.scoreboard.interval
            }
    in
    ( model, Cmd.batch [ fetchScores ] )


view : Model -> Browser.Document Msg
view model =
    { title = "Git Challenge ScoreBoard"
    , body = [ Board.view model ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlRequest (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        OnUrlRequest (Browser.External link) ->
            ( model, Nav.load link )

        OnUrlChange url ->
            ( { model | url = url }, Cmd.none )

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

        FetchScores (Ok scores) ->
            ( { model | scores = Success scores }, Cmd.none )

        FetchScores (Err _) ->
            ( model, Cmd.none )


fetchScores : Cmd Msg
fetchScores =
    API.getApiScores FetchScores


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every model.interval Tick
