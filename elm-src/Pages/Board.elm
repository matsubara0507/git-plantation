module Pages.Board exposing (view)

import Browser as Browser
import Generated.API as API exposing (..)
import Html exposing (..)
import Html.Attributes exposing (checked, class, href, id, style, target, type_)
import Html.Events exposing (onCheck, onClick)
import Http
import Score exposing (Score)
import Time exposing (Posix)


type alias Model a =
    { a
        | problems : List API.Problem
        , scores : List Score
    }


view : Model a -> Html msg
view model =
    div [ id "scoreboard" ]
        [ table
            [ class "scoreboard-table col-12 f3" ]
            [ thead [] [ tr [ class "border-bottum" ] (viewHeader model) ]
            , tbody [] (viewBody model)
            ]
        ]


viewHeader : Model a -> List (Html msg)
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


viewBody : Model a -> List (Html msg)
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
