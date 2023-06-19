module Pages.Board exposing (view)

import Generated.API as API exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, href, id, style, target)
import Score exposing (Score)


type alias Model a =
    { a
        | problems : List API.Problem
        , scores : List Score
    }


view : Model a -> Html msg
view model =
    div [ id "scoreboard" ]
        [ table
            [ class "scoreboard-table col-12 f5 break-word", style "table-layout" "fixed" ]
            [ thead [] [ tr [ class "border-bottum" ] (viewHeader model) ]
            , tbody [] (viewBody model)
            ]
        ]


viewHeader : Model a -> List (Html msg)
viewHeader model =
    List.concat
        [ [ th [] [] ]
        , List.map viewHeaderCol model.problems
        , [ th [ class "text-center p-2 f6" ] [ text "Score" ] ]
        ]


viewHeaderCol : API.Problem -> Html msg
viewHeaderCol problem =
    th
        [ id problem.name, class "text-center p-2 f6" ]
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
                "color-bg-inset"

             else
                ""
            )
        ]
        (List.concat
            [ [ th [ class "text-right p-2 f" ]
                    [ a [ class "Link--primary", href ("/teams/" ++ score.team.id) ]
                        [ text score.team.name ]
                    ]
              ]
            , List.map viewStatus score.stats
            , [ th [ class "text-center p-2 f6" ] [ text (String.fromInt score.point) ] ]
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
            span [ class "State State--small" ] [ text "未提出" ]

        Score.Pending ->
            span [ class "State State--small color-bg-attention-emphasis" ] [ text "採点中" ]

        Score.Incorrect ->
            span [ class "State State--small color-bg-danger-emphasis" ] [ text "不正解" ]

        Score.Correct ->
            span [ class "State State--small color-bg-success-emphasis" ] [ text "正解" ]


stars : Int -> Html msg
stars n =
    let
        star =
            i [ class "fas fa-star" ] []
    in
    if n < 4 then
        div [ class "f6" ] (List.repeat n star)

    else
        div [ class "f6" ] [ star, text ("x" ++ String.fromInt n) ]
