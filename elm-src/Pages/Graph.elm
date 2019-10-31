module Pages.Graph exposing (Model, Msg(..), init, update, view)

import Browser as Browser
import Color exposing (Color)
import Dict
import Generated.API as API exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onCheck, onClick)
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
import Score exposing (Score)
import Time exposing (Posix, Zone)
import TimeZone


type alias Global a =
    { a
        | config : API.ScoreBoardConfig
        , scores : List Score
    }


type alias Model =
    { zone : Zone
    , hinted : Maybe ScoreHistory
    }


type alias ScoreHistory =
    { point : Int
    , latest : Maybe Score.Status
    }


type Msg
    = Hint (Maybe ScoreHistory)


init : Global a -> Model
init model =
    let
        zone =
            model.config.zone
                |> Maybe.andThen (\k -> Dict.get k TimeZone.zones)
                |> Maybe.withDefault (\_ -> Time.utc)
    in
    { hinted = Nothing, zone = zone () }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Hint point ->
            ( { model | hinted = point }, Cmd.none )


view : Global a -> Model -> Html.Html Msg
view global model =
    Html.div [ style "margin-top" "1em" ] [ chart global model ]


chart : Global a -> Model -> Html.Html Msg
chart global model =
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
                , variable = Maybe.map toFloat << Maybe.map (\n -> n * 1000) << Maybe.map .correctTime << .latest
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
                [ ( "Problem", Maybe.withDefault "" << Maybe.map .problemName << .latest )
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
        (buildData global.scores)


buildData : List Score -> List (LineChart.Series ScoreHistory)
buildData scores =
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
    List.sortBy (.name << .team) scores
        |> List.map2 buildScoreHistories colors


buildScoreHistories : Color -> Score -> LineChart.Series ScoreHistory
buildScoreHistories color score =
    score.stats
        |> List.filter (\s -> s.state == Score.Correct)
        |> List.sortBy .correctTime
        |> List.scanl (::) []
        |> List.map buildScoreHistory
        |> LineChart.line color Dots.circle score.team.name


buildScoreHistory : List Score.Status -> ScoreHistory
buildScoreHistory stats =
    { point = List.sum (List.map .difficulty stats)
    , latest = List.head stats
    }


toCorrectTime : Zone -> ScoreHistory -> String
toCorrectTime zone score =
    let
        time =
            score.latest
                |> Maybe.map .correctTime
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
