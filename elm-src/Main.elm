module Main exposing (Model, Msg(..), init, main, subscriptions, update, view, viewCheckReload)

import Browser as Browser
import Generated.API as API exposing (..)
import Html exposing (..)
import Html.Attributes exposing (checked, class, id, style, type_)
import Html.Events exposing (onCheck, onClick)
import Http
import List.Extra as List
import RemoteData exposing (RemoteData(..))
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
    , scores : RemoteData String (List API.Score)
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
            , scores = NotAsked
            }
    in
    ( model, Cmd.batch [ fetchScores ] )


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
    case model.scores of
        Success scores ->
            List.indexedMap (viewScore model.problems) scores

        _ ->
            []


viewScore : List API.Problem -> Int -> API.Score -> Html msg
viewScore problems idx score =
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
            [ [ th [ class "text-right p-2 f4" ] [ text score.team ] ]
            , List.map (viewStatus score.stats) problems
            , [ th [ class "text-center p-2 f4" ] [ text (String.fromInt score.point) ] ]
            ]
        )


viewStatus : List API.Status -> API.Problem -> Html msg
viewStatus stats problem =
    let
        status =
            List.find (\st -> st.problem == problem.name) stats
    in
    th
        [ class "text-center p-2" ]
        [ div [] [ statBadge status ]
        , div [] [ stars problem.difficulty ]
        ]


statBadge : Maybe API.Status -> Html msg
statBadge status =
    case Maybe.map .correct status of
        Nothing ->
            span [ class "Label Label--gray-darker" ] [ text "未提出" ]

        Just False ->
            span [ class "Label bg-red" ] [ text "不正解" ]

        Just True ->
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
            ( { model | scores = Failure "Something went wrong.." }, Cmd.none )


fetchScores : Cmd Msg
fetchScores =
    Http.send FetchScores API.getApiScores


baseUrl : String
baseUrl =
    "localhost:8080"


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 60000 Tick
