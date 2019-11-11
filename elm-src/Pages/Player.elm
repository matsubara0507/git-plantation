module Pages.Player exposing (Model, Msg, init, update, view)

import Generated.API as API exposing (..)
import Html exposing (Html)
import Pages.Board as Board
import Pages.Graph as Graph
import Pages.Team as Team
import Score exposing (Score)


type alias Model =
    { id : String, teamID : String, graph : Graph.Model }


type alias Global a =
    { a
        | config : API.ScoreBoardConfig
        , problems : List API.Problem
        , scores : List Score
    }


type alias Msg =
    Graph.Msg


init : Global a -> String -> String -> Model
init global teamID id =
    { id = id, teamID = teamID, graph = Graph.init global }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( graph, newMsg ) =
            Graph.update msg model.graph
    in
    ( { model | graph = graph }, newMsg )


view : Global a -> Model -> Html Msg
view global model =
    let
        filtered =
            { global | scores = Score.filterByPlayerID model.id global.scores }
    in
    Html.div []
        [ Board.view filtered
        , Team.viewFilters filtered { id = model.teamID, graph = model.graph }
        , Graph.view filtered model.graph
        ]
