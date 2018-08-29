module Generated.API exposing (Config, Problem, Score, Status, Team, decodeConfig, decodeProblem, decodeScore, decodeStatus, decodeTeam, encodeConfig, encodeProblem, encodeScore, encodeStatus, encodeTeam, getApiProblems, getApiScores, getApiTeams)

import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import String


type alias Team =
    { name : String
    , github : String
    , member : List String
    }


decodeTeam : Decoder Team
decodeTeam =
    Json.Decode.succeed Team
        |> required "name" string
        |> required "github" string
        |> required "member" (list string)


encodeTeam : Team -> Json.Encode.Value
encodeTeam x =
    Json.Encode.object
        [ ( "name", Json.Encode.string x.name )
        , ( "github", Json.Encode.string x.github )
        , ( "member", Json.Encode.list Json.Encode.string x.member )
        ]


type alias Problem =
    { problem_name : String
    , repo_name : String
    , difficulty : Int
    , challenge_branches : List String
    , ci_branch : String
    }


decodeProblem : Decoder Problem
decodeProblem =
    Json.Decode.succeed Problem
        |> required "problem_name" string
        |> required "repo_name" string
        |> required "difficulty" int
        |> required "challenge_branches" (list string)
        |> required "ci_branch" string


encodeProblem : Problem -> Json.Encode.Value
encodeProblem x =
    Json.Encode.object
        [ ( "problem_name", Json.Encode.string x.problem_name )
        , ( "repo_name", Json.Encode.string x.repo_name )
        , ( "difficulty", Json.Encode.int x.difficulty )
        , ( "challenge_branches", Json.Encode.list Json.Encode.string x.challenge_branches )
        , ( "ci_branch", Json.Encode.string x.ci_branch )
        ]


type alias Config =
    { problems : List Problem
    , teams : List Team
    }


decodeConfig : Decoder Config
decodeConfig =
    Json.Decode.succeed Config
        |> required "problems" (list decodeProblem)
        |> required "teams" (list decodeTeam)


encodeConfig : Config -> Json.Encode.Value
encodeConfig x =
    Json.Encode.object
        [ ( "problems", Json.Encode.list encodeProblem x.problems )
        , ( "teams", Json.Encode.list encodeTeam x.teams )
        ]


type alias Score =
    { team : String
    , point : Int
    , stats : List Status
    }


decodeScore : Decoder Score
decodeScore =
    Json.Decode.succeed Score
        |> required "team" string
        |> required "point" int
        |> required "stats" (list decodeStatus)


encodeScore : Score -> Json.Encode.Value
encodeScore x =
    Json.Encode.object
        [ ( "team", Json.Encode.string x.team )
        , ( "point", Json.Encode.int x.point )
        , ( "stats", Json.Encode.list encodeStatus x.stats )
        ]


type alias Status =
    { problem : String
    , correct : Bool
    }


decodeStatus : Decoder Status
decodeStatus =
    Json.Decode.succeed Status
        |> required "problem" string
        |> required "correct" bool


encodeStatus : Status -> Json.Encode.Value
encodeStatus x =
    Json.Encode.object
        [ ( "problem", Json.Encode.string x.problem )
        , ( "correct", Json.Encode.bool x.correct )
        ]


getApiTeams : Http.Request (List Team)
getApiTeams =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "teams"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeTeam)
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getApiProblems : Http.Request (List Problem)
getApiProblems =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "problems"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeProblem)
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getApiScores : Http.Request (List Score)
getApiScores =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "scores"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeScore)
        , timeout =
            Nothing
        , withCredentials =
            False
        }
