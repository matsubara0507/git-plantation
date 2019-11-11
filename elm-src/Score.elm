module Score exposing (Score, State(..), Status, Team, build, filterByPlayerID, filterByTeamIDs, updateBy)

import Generated.API as API
import List.Extra as List


type alias Score =
    { team : Team
    , point : Int
    , stats : List Status
    }


type alias Team =
    { id : String
    , name : String
    , member : List API.User
    }


type alias Status =
    { problemId : Int
    , problemName : String
    , difficulty : Int
    , url : String
    , state : State
    , correctTime : Int -- unixtime
    , answerer : String
    }


type State
    = None
    | Pending
    | Incorrect
    | Correct


build : { a | problems : List API.Problem, teams : List API.Team } -> List API.Score -> List Score
build { problems, teams } scores =
    List.map (buildEmptyScore problems) teams
        |> updateBy scores


buildEmptyScore : List API.Problem -> API.Team -> Score
buildEmptyScore problems team =
    { team = { id = team.id, name = team.name, member = team.member }
    , point = 0
    , stats = List.map buildEmptyStatus problems
    }


buildEmptyStatus : API.Problem -> Status
buildEmptyStatus problem =
    { problemId = problem.id
    , problemName = problem.name
    , difficulty = problem.difficulty
    , url = ""
    , state = None
    , correctTime = 0
    , answerer = ""
    }


updateBy : List API.Score -> List Score -> List Score
updateBy respScores scores =
    for scores
        (\score ->
            case List.find (\s -> s.team == score.team.id) respScores of
                Nothing ->
                    score

                Just resp ->
                    updateScoreBy resp score
        )


updateScoreBy : API.Score -> Score -> Score
updateScoreBy respScore score =
    { score
        | point = respScore.point
        , stats = List.map (updateStatsBy respScore) score.stats
    }


updateStatsBy : API.Score -> Status -> Status
updateStatsBy respScore status =
    let
        url =
            respScore.links
                |> List.find (\l -> l.problem_id == status.problemId)
                |> Maybe.map .url
                |> Maybe.withDefault status.url

        respStatus =
            List.find (\s -> s.problem_id == status.problemId) respScore.stats

        state =
            respStatus
                |> Maybe.map toState
                |> Maybe.withDefault status.state

        correctTime =
            respStatus
                |> Maybe.andThen .corrected_at
                |> Maybe.withDefault status.correctTime

        answerer =
            respStatus
                |> Maybe.andThen .answerer
                |> Maybe.withDefault ""
    in
    { status | url = url, state = state, correctTime = correctTime, answerer = answerer }


toState : API.Status -> State
toState status =
    case ( status.correct, status.pending ) of
        ( _, True ) ->
            Pending

        ( False, _ ) ->
            Incorrect

        ( True, _ ) ->
            Correct


filterByTeamIDs : List String -> List Score -> List Score
filterByTeamIDs ids =
    List.filter (\s -> List.member s.team.id ids)


filterByPlayerID : String -> List Score -> List Score
filterByPlayerID idx scores =
    scores
        |> List.filter (\score -> List.member idx (List.map .github score.team.member))
        |> List.map
            (\score ->
                let
                    team =
                        score.team

                    filtered =
                        { team | member = List.filter (\u -> u.github == idx) team.member }
                in
                { score
                    | point = 0
                    , stats = List.map (updateStatusForPlayer idx) score.stats
                    , team = filtered
                }
            )


updateStatusForPlayer : String -> Status -> Status
updateStatusForPlayer idx status =
    if status.answerer == idx then
        status

    else
        { status | state = None }



-- Util


for : List a -> (a -> b) -> List b
for xs f =
    List.map f xs
