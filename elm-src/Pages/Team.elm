module Pages.Team exposing (Model, Msg, init, update, view, viewFilters)

import Generated.API as API exposing (..)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, href)
import Pages.Board as Board
import Pages.Graph as Graph
import Score exposing (Score)


type alias Model =
    { id : String, graph : Graph.Model }


type alias Global a =
    { a
        | config : API.ScoreBoardConfig
        , problems : List API.Problem
        , scores : List Score
    }


type alias Msg =
    Graph.Msg


init : Global a -> String -> Model
init global id =
    { id = id, graph = Graph.init global }


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
            { global | scores = Score.filterByTeamIDs [ model.id ] global.scores }
    in
    Html.div []
        [ Board.view filtered
        , viewFilters filtered model
        , Graph.view filtered model.graph
        ]


viewFilters : Global a -> Model -> Html msg
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
