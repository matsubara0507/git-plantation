module Generated.API exposing (Problem, Team, decodeProblem, decodeTeam, encodeProblem, encodeTeam, getApiProblems, getApiTeams)

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
    decode Team
        |> required "name" string
        |> required "github" string
        |> required "member" (list string)


encodeTeam : Team -> Json.Encode.Value
encodeTeam x =
    Json.Encode.object
        [ ( "name", Json.Encode.string x.name )
        , ( "github", Json.Encode.string x.github )
        , ( "member", (Json.Encode.list << List.map Json.Encode.string) x.member )
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
    decode Problem
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
        , ( "challenge_branches", (Json.Encode.list << List.map Json.Encode.string) x.challenge_branches )
        , ( "ci_branch", Json.Encode.string x.ci_branch )
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
