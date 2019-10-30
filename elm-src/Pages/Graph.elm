module Main exposing (main)

import Browser as Browser
import Color exposing (Color)
import Dict
import Generated.API as API exposing (..)
import Html exposing (..)
import Html.Attributes exposing (checked, class, href, id, style, target, type_)
import Html.Events exposing (onCheck, onClick)
import Http
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Axis.Line as AxisLine
import LineChart.Axis.Range as Range
import LineChart.Axis.Ticks as Ticks
import LineChart.Axis.Title as Title
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Coordinate as Coordinate
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import List.Extra as List
import Palette.Cubehelix as Palette
import Palette.Tango as Palette
import RemoteData exposing (RemoteData(..))
import Time exposing (Posix, Zone)
import TimeZone


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { reload : Bool
    , problems : List API.Problem
    , teams : List API.Team
    , scores : RemoteData String (List API.Score)
    , interval : Float
    , hinted : Maybe ScoreHistory
    , zone : Zone
    }


type alias ScoreHistory =
    { point : Int
    , latest : Maybe API.Status
    }


type Msg
    = CheckReload Bool
    | Reload
    | Tick Posix
    | FetchScores (Result Http.Error (List API.Score))
    | Hint (Maybe ScoreHistory)


type alias Flags =
    { config : API.Config }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        zone =
            flags.config.scoreboard.zone
                |> Maybe.andThen (\k -> Dict.get k TimeZone.zones)
                |> Maybe.withDefault (\_ -> Time.utc)

        model =
            { reload = True
            , problems = flags.config.problems
            , teams = flags.config.teams
            , scores = NotAsked
            , interval = flags.config.scoreboard.interval
            , hinted = Nothing
            , zone = zone ()
            }
    in
    ( model, Cmd.batch [ API.getApiScores FetchScores ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        FetchScores (Ok scores) ->
            ( { model | scores = Success scores }, Cmd.none )

        FetchScores (Err _) ->
            ( model, Cmd.none )

        Hint point ->
            ( { model | hinted = point }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every model.interval Tick


view : Model -> Html Msg
view model =
    div [ class "" ]
        [ div [ class "my-3 mx-auto col-10" ]
            [ div []
                [ h2
                    [ class "f1-light float-left link-gray-dark"
                    , onClick Reload
                    ]
                    [ text "Git Challenge ScoreBoard" ]
                , div [ class "float-right" ] [ viewCheckReload model ]
                ]
            , viewGrpah model
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


viewGrpah : Model -> Html.Html Msg
viewGrpah model =
    Html.div [ style "margin-top" "1em" ] [ chart model ]


chart : Model -> Html.Html Msg
chart model =
    LineChart.viewCustom
        { y =
            Axis.custom
                { title = Title.default "Point"
                , variable = Just << toFloat << .point
                , pixels = 380
                , range = Range.padded 20 20
                , axisLine = AxisLine.full Colors.gray
                , ticks = Ticks.float 5
                }
        , x =
            Axis.custom
                { title = Title.default "Time"
                , variable = Maybe.map toFloat << Maybe.map (\n -> n * 1000) << Maybe.andThen .corrected_at << .latest
                , pixels = 1270
                , range = Range.padded 20 20
                , axisLine = AxisLine.full Colors.gray
                , ticks = Ticks.time model.zone 10
                }
        , container =
            Container.custom
                { attributesHtml = []
                , attributesSvg = []
                , size = Container.relative
                , margin = Container.Margin 30 140 30 70
                , id = "line-chart-stepped"
                }
        , interpolation = Interpolation.stepped
        , intersection = Intersection.default
        , legends = Legends.default
        , events = Events.hoverOne Hint
        , junk =
            Junk.hoverOne model.hinted
                [ ( "Problem", findProblemName model.problems )
                , ( "Time", toCorrectTime model.zone )
                ]
        , grid = Grid.default
        , area = Area.default
        , line = Line.default
        , dots =
            let
                styleLegend _ =
                    Dots.empty 5 1

                styleIndividual datum =
                    if Just datum == model.hinted then
                        Dots.full 5

                    else
                        Dots.empty 5 1
            in
            Dots.customAny
                { legend = styleLegend
                , individual = styleIndividual
                }
        }
        (buildData model)


buildData : Model -> List (LineChart.Series ScoreHistory)
buildData model =
    case model.scores of
        Success scores ->
            let
                colors =
                    Palette.generateAdvanced (List.length scores + 4)
                        { start = Color.fromHSL ( 0, 100, 50 )
                        , rotationDirection = Palette.RGB
                        , rotations = 1.5
                        , gamma = 1.2
                        }
                        |> List.drop 2
                        |> List.take (List.length scores)
            in
            List.sortBy .team scores
                |> List.map2 (buildScoreHistories model) colors

        _ ->
            []


buildScoreHistories : Model -> Color -> API.Score -> LineChart.Series ScoreHistory
buildScoreHistories model color score =
    score.stats
        |> List.filter .correct
        |> List.sortBy (Maybe.withDefault 0 << .corrected_at)
        |> List.scanl (::) []
        |> List.map (buildScoreHistory model)
        |> LineChart.line color Dots.circle score.team


buildScoreHistory : Model -> List API.Status -> ScoreHistory
buildScoreHistory model stats =
    { point = List.sum (List.map (findProblemPoint model) stats)
    , latest = List.head stats
    }


findProblemPoint : Model -> API.Status -> Int
findProblemPoint model status =
    model.problems
        |> List.find (\p -> p.id == status.problem_id)
        |> Maybe.map .difficulty
        |> Maybe.withDefault 0


toCorrectTime : Zone -> ScoreHistory -> String
toCorrectTime zone score =
    let
        time =
            score.latest
                |> Maybe.andThen .corrected_at
                |> Maybe.map (\n -> n * 1000)
                |> Maybe.map Time.millisToPosix
    in
    String.concat
        [ Maybe.map (Time.toHour zone) time
            |> Maybe.withDefault 0
            |> String.fromInt
            |> String.padLeft 2 '0'
        , ":"
        , Maybe.map (Time.toMinute zone) time
            |> Maybe.withDefault 0
            |> String.fromInt
            |> String.padLeft 2 '0'
        , ":"
        , Maybe.map (Time.toSecond zone) time
            |> Maybe.withDefault 0
            |> String.fromInt
            |> String.padLeft 2 '0'
        ]


findProblemName : List API.Problem -> ScoreHistory -> String
findProblemName problems score =
    case score.latest of
        Nothing ->
            "none"

        Just status ->
            problems
                |> List.find (\p -> p.id == status.problem_id)
                |> Maybe.map .name
                |> Maybe.withDefault "none"
