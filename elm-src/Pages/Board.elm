module Main exposing (main)

import Browser as Browser
import Generated.API as API exposing (..)
import Html exposing (..)
import Html.Attributes exposing (checked, class, href, id, style, target, type_)
import Html.Events exposing (onCheck, onClick)
import Http
import Score exposing (Score)
import Time exposing (Posix)


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
    , scores : List Score
    , interval : Float
    }


type Msg
    = CheckReload Bool
    | Reload
    | Tick Posix
    | FetchScores (Result Http.Error (List API.Score))


type alias Flags =
    { config : API.Config }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { reload = True
            , problems = flags.config.problems
            , teams = flags.config.teams
            , scores = []
            , interval = flags.config.scoreboard.interval
            }
    in
    ( { model | scores = Score.build model [] }, Cmd.batch [ API.getApiScores FetchScores ] )


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
    div [ id "scoreboard" ]
        [ table
            [ class "scoreboard-table col-12 f3" ]
            [ thead [] [ tr [ class "border-bottum" ] (viewHeader model) ]
            , tbody [] (viewBody model)
            ]
        ]


viewHeader : Model -> List (Html msg)
viewHeader model =
    List.concat
        [ [ th [] [] ]
        , List.map viewHeaderCol model.problems
        , [ th [ class "text-center p-2 f4" ] [ text "Score" ] ]
        ]


viewHeaderCol : API.Problem -> Html msg
viewHeaderCol problem =
    th
        [ id problem.name, class "text-center p-2 f4", style "width" "100px" ]
        [ text problem.name ]


viewBody : Model -> List (Html msg)
viewBody model =
    List.indexedMap viewScore model.scores


viewScore : Int -> Score -> Html msg
viewScore idx score =
    tr
        [ class "border-top"
        , class
            (if modBy 2 idx == 0 then
                "bg-gray-light"

             else
                ""
            )
        ]
        (List.concat
            [ [ th [ class "text-right p-2 f4" ] [ text score.team.name ] ]
            , List.map viewStatus score.stats
            , [ th [ class "text-center p-2 f4" ] [ text (String.fromInt score.point) ] ]
            ]
        )


viewStatus : Score.Status -> Html msg
viewStatus status =
    th
        [ class "text-center p-2" ]
        [ a [ href status.url, target "_blank" ] [ statBadge status.state ]
        , div [] [ stars status.difficulty ]
        ]


statBadge : Score.State -> Html msg
statBadge state =
    case state of
        Score.None ->
            span [ class "Label Label--gray-darker" ] [ text "未提出" ]

        Score.Pending ->
            span [ class "Label bg-yellow" ] [ text "採点中" ]

        Score.Incorrect ->
            span [ class "Label bg-red" ] [ text "不正解" ]

        Score.Correct ->
            span [ class "Label bg-green" ] [ text "正解" ]


stars : Int -> Html msg
stars n =
    let
        star =
            i [ class "fas fa-star" ] []
    in
    if n < 4 then
        div [ class "f5" ] (List.repeat n star)

    else
        div [ class "f5" ] [ star, text ("x" ++ String.fromInt n) ]


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

        FetchScores (Ok resp) ->
            ( { model | scores = Score.updateBy resp model.scores }, Cmd.none )

        FetchScores (Err _) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every model.interval Tick
