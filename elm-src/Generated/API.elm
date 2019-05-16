module Generated.API exposing (Config, Link, Problem, Repo, Score, ScoreBoardConfig, Status, Team, User, decodeConfig, decodeLink, decodeProblem, decodeRepo, decodeScore, decodeScoreBoardConfig, decodeStatus, decodeTeam, decodeUser, encodeConfig, encodeLink, encodeProblem, encodeRepo, encodeScore, encodeScoreBoardConfig, encodeStatus, encodeTeam, encodeUser, getApiProblems, getApiScores, getApiTeams)

import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import String


type alias Team =
    { id : String
    , name : String
    , repos : List Repo
    , member : List User
    , org : Maybe String
    , gh_teams : List String
    }


decodeTeam : Decoder Team
decodeTeam =
    Json.Decode.succeed Team
        |> required "id" string
        |> required "name" string
        |> required "repos" (list decodeRepo)
        |> required "member" (list decodeUser)
        |> required "org" (maybe string)
        |> required "gh_teams" (list string)


encodeTeam : Team -> Json.Encode.Value
encodeTeam x =
    Json.Encode.object
        [ ( "id", Json.Encode.string x.id )
        , ( "name", Json.Encode.string x.name )
        , ( "repos", Json.Encode.list encodeRepo x.repos )
        , ( "member", Json.Encode.list encodeUser x.member )
        , ( "org", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.org )
        , ( "gh_teams", Json.Encode.list Json.Encode.string x.gh_teams )
        ]


type alias User =
    { name : String
    , github : String
    }


decodeUser : Decoder User
decodeUser =
    Json.Decode.succeed User
        |> required "name" string
        |> required "github" string


encodeUser : User -> Json.Encode.Value
encodeUser x =
    Json.Encode.object
        [ ( "name", Json.Encode.string x.name )
        , ( "github", Json.Encode.string x.github )
        ]


type alias Repo =
    { name : String
    , owner : Maybe String
    , org : Maybe String
    , problem : Int
    , private : Bool
    , only : Maybe String
    }


decodeRepo : Decoder Repo
decodeRepo =
    Json.Decode.succeed Repo
        |> required "name" string
        |> required "owner" (maybe string)
        |> required "org" (maybe string)
        |> required "problem" int
        |> required "private" bool
        |> required "only" (maybe string)


encodeRepo : Repo -> Json.Encode.Value
encodeRepo x =
    Json.Encode.object
        [ ( "name", Json.Encode.string x.name )
        , ( "owner", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.owner )
        , ( "org", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.org )
        , ( "problem", Json.Encode.int x.problem )
        , ( "private", Json.Encode.bool x.private )
        , ( "only", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.only )
        ]


type alias Problem =
    { id : Int
    , name : String
    , repo : String
    , difficulty : Int
    , challenge_branches : List String
    , answer_branch : String
    , ci_branch : String
    , default_branch : String
    }


decodeProblem : Decoder Problem
decodeProblem =
    Json.Decode.succeed Problem
        |> required "id" int
        |> required "name" string
        |> required "repo" string
        |> required "difficulty" int
        |> required "challenge_branches" (list string)
        |> required "answer_branch" string
        |> required "ci_branch" string
        |> required "default_branch" string


encodeProblem : Problem -> Json.Encode.Value
encodeProblem x =
    Json.Encode.object
        [ ( "id", Json.Encode.int x.id )
        , ( "name", Json.Encode.string x.name )
        , ( "repo", Json.Encode.string x.repo )
        , ( "difficulty", Json.Encode.int x.difficulty )
        , ( "challenge_branches", Json.Encode.list Json.Encode.string x.challenge_branches )
        , ( "answer_branch", Json.Encode.string x.answer_branch )
        , ( "ci_branch", Json.Encode.string x.ci_branch )
        , ( "default_branch", Json.Encode.string x.default_branch )
        ]


type alias Config =
    { scoreboard : ScoreBoardConfig
    , problems : List Problem
    , teams : List Team
    }


decodeConfig : Decoder Config
decodeConfig =
    Json.Decode.succeed Config
        |> required "scoreboard" decodeScoreBoardConfig
        |> required "problems" (list decodeProblem)
        |> required "teams" (list decodeTeam)


encodeConfig : Config -> Json.Encode.Value
encodeConfig x =
    Json.Encode.object
        [ ( "scoreboard", encodeScoreBoardConfig x.scoreboard )
        , ( "problems", Json.Encode.list encodeProblem x.problems )
        , ( "teams", Json.Encode.list encodeTeam x.teams )
        ]


type alias ScoreBoardConfig =
    { interval : Float
    }


decodeScoreBoardConfig : Decoder ScoreBoardConfig
decodeScoreBoardConfig =
    Json.Decode.succeed ScoreBoardConfig
        |> required "interval" float


encodeScoreBoardConfig : ScoreBoardConfig -> Json.Encode.Value
encodeScoreBoardConfig x =
    Json.Encode.object
        [ ( "interval", Json.Encode.float x.interval )
        ]


type alias Score =
    { team : String
    , point : Int
    , stats : List Status
    , links : List Link
    }


decodeScore : Decoder Score
decodeScore =
    Json.Decode.succeed Score
        |> required "team" string
        |> required "point" int
        |> required "stats" (list decodeStatus)
        |> required "links" (list decodeLink)


encodeScore : Score -> Json.Encode.Value
encodeScore x =
    Json.Encode.object
        [ ( "team", Json.Encode.string x.team )
        , ( "point", Json.Encode.int x.point )
        , ( "stats", Json.Encode.list encodeStatus x.stats )
        , ( "links", Json.Encode.list encodeLink x.links )
        ]


type alias Status =
    { problem : String
    , correct : Bool
    , pending : Bool
    }


decodeStatus : Decoder Status
decodeStatus =
    Json.Decode.succeed Status
        |> required "problem" string
        |> required "correct" bool
        |> required "pending" bool


encodeStatus : Status -> Json.Encode.Value
encodeStatus x =
    Json.Encode.object
        [ ( "problem", Json.Encode.string x.problem )
        , ( "correct", Json.Encode.bool x.correct )
        , ( "pending", Json.Encode.bool x.pending )
        ]


type alias Link =
    { problem_id : Int
    , url : String
    }


decodeLink : Decoder Link
decodeLink =
    Json.Decode.succeed Link
        |> required "problem_id" int
        |> required "url" string


encodeLink : Link -> Json.Encode.Value
encodeLink x =
    Json.Encode.object
        [ ( "problem_id", Json.Encode.int x.problem_id )
        , ( "url", Json.Encode.string x.url )
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
