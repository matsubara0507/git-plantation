module Pages.Team exposing (Model, Msg, init, update, view, viewFilters)

import Generated.API as API exposing (..)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, href, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Pages.Board as Board
import Pages.Graph as Graph
import Score exposing (Score)


type alias Model =
    { id : String
    , graph : Graph.Model
    , selected : Maybe API.Problem
    , selectFilter : String
    }


type alias Global a =
    { a
        | config : API.ScoreBoardConfig
        , problems : List API.Problem
        , scores : List Score
    }


type Msg
    = GraphMsg Graph.Msg
    | InputSelectFilter String
    | SelectProblem (Maybe API.Problem)
    | ClickResetButton
    | ResetRepository (Result Http.Error ())


init : Global a -> String -> Model
init global id =
    { id = id
    , graph = Graph.init global
    , selected = Nothing
    , selectFilter = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        GraphMsg msg ->
            let
                ( graph, newMsg ) =
                    Graph.update msg model.graph
            in
            ( { model | graph = graph }, Cmd.map GraphMsg newMsg )

        InputSelectFilter input ->
            ( { model | selectFilter = input }, Cmd.none )

        SelectProblem problem ->
            ( { model | selected = problem }, Cmd.none )

        ClickResetButton ->
            case model.selected of
                Just problem ->
                    ( model, API.postApiResetByTeamByProbrem model.id problem.id ResetRepository )

                Nothing ->
                    ( model, Cmd.none )

        ResetRepository _ ->
            ( model, Cmd.none )


view : Global a -> Model -> Html Msg
view global model =
    let
        filtered =
            { global | scores = Score.filterByTeamIDs [ model.id ] global.scores }
    in
    Html.div []
        [ Board.view filtered
        , if Maybe.withDefault True global.config.resetable then
            viewReset global.problems model
          else
            div [] []
        , viewFilters filtered model
        , Html.map GraphMsg (Graph.view filtered model.graph)
        ]


type alias FilterModel a =
    { a | id : String }


viewFilters : Global a -> FilterModel b -> Html msg
viewFilters global model =
    let
        toTag sec idx =
            a [ href ("/" ++ sec ++ "/" ++ idx), class "branch-name" ]
                [ text (sec ++ ":" ++ idx) ]

        members =
            global.scores
                |> List.concatMap (\s -> s.team.member)
                |> List.map (\player -> toTag ("teams/" ++ model.id) player.github)
    in
    div [ class "m-3" ] (List.concat [ members ])


type alias ResetModel a =
    { a | id : String, selected : Maybe API.Problem, selectFilter : String }


viewReset : List API.Problem -> ResetModel a -> Html Msg
viewReset problems model =
    div [ class "form-group d-flex flex-justify-end" ]
        [ details [ class "details-reset details-overlay Truncate mr-1" ]
            [ summary [ class "form-select select-sm Truncate-text" ]
                [ model.selected
                    |> Maybe.map .name
                    |> Maybe.withDefault ""
                    |> text
                ]
            , div [ class "SelectMenu SelectMenu--hasFilter right-12" ]
                [ let
                    filtered =
                        filterSelectOption model problems
                  in
                  div [ class "SelectMenu-modal" ]
                    [ form [ class "SelectMenu-filter" ]
                        [ input
                            [ class "SelectMenu-input form-control"
                            , type_ "text"
                            , placeholder "Search"
                            , attribute "aria-label" "Filter"
                            , onInput InputSelectFilter
                            ]
                            []
                        ]
                    , div [ class "SelectMenu-list" ]
                        (List.map (\p -> div [ class "SelectMenu-item", onClick (SelectProblem (Just p)) ] [ text p.name ]) filtered)
                    , footer [ class "SelectMenu-footer" ]
                        [ text ("Showing " ++ String.fromInt (List.length filtered) ++ " of " ++ String.fromInt (List.length problems)) ]
                    ]
                ]
            ]
        , button [ class "btn btn-sm btn-danger", onClick ClickResetButton ] [ text "Reset" ]
        ]


filterSelectOption : ResetModel a -> List API.Problem -> List API.Problem
filterSelectOption model problems =
    if String.isEmpty model.selectFilter then
        problems

    else
        List.filter (\p -> String.contains model.selectFilter p.name) problems
