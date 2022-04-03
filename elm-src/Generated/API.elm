module Generated.API exposing(..)
import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String
import Url.Builder

type alias Team  =
   { id: String
   , name: String
   , repos: (List Repo)
   , member: (List User)
   , org: (Maybe String)
   , gh_teams: (List String)
   }

jsonDecTeam : Json.Decode.Decoder ( Team )
jsonDecTeam =
   Json.Decode.succeed (\pid pname prepos pmember porg pgh_teams -> {id = pid, name = pname, repos = prepos, member = pmember, org = porg, gh_teams = pgh_teams})
   |> required "id" (Json.Decode.string)
   |> required "name" (Json.Decode.string)
   |> required "repos" (Json.Decode.list (jsonDecRepo))
   |> required "member" (Json.Decode.list (jsonDecUser))
   |> fnullable "org" (Json.Decode.string)
   |> required "gh_teams" (Json.Decode.list (Json.Decode.string))

jsonEncTeam : Team -> Value
jsonEncTeam  val =
   Json.Encode.object
   [ ("id", Json.Encode.string val.id)
   , ("name", Json.Encode.string val.name)
   , ("repos", (Json.Encode.list jsonEncRepo) val.repos)
   , ("member", (Json.Encode.list jsonEncUser) val.member)
   , ("org", (maybeEncode (Json.Encode.string)) val.org)
   , ("gh_teams", (Json.Encode.list Json.Encode.string) val.gh_teams)
   ]



type alias User  =
   { name: String
   , github: String
   }

jsonDecUser : Json.Decode.Decoder ( User )
jsonDecUser =
   Json.Decode.succeed (\pname pgithub -> {name = pname, github = pgithub})
   |> required "name" (Json.Decode.string)
   |> required "github" (Json.Decode.string)

jsonEncUser : User -> Value
jsonEncUser  val =
   Json.Encode.object
   [ ("name", Json.Encode.string val.name)
   , ("github", Json.Encode.string val.github)
   ]



type alias Repo  =
   { name: String
   , owner: (Maybe String)
   , org: (Maybe String)
   , problem: Int
   , private: Bool
   , only: (Maybe String)
   }

jsonDecRepo : Json.Decode.Decoder ( Repo )
jsonDecRepo =
   Json.Decode.succeed (\pname powner porg pproblem pprivate ponly -> {name = pname, owner = powner, org = porg, problem = pproblem, private = pprivate, only = ponly})
   |> required "name" (Json.Decode.string)
   |> fnullable "owner" (Json.Decode.string)
   |> fnullable "org" (Json.Decode.string)
   |> required "problem" (Json.Decode.int)
   |> required "private" (Json.Decode.bool)
   |> fnullable "only" (Json.Decode.string)

jsonEncRepo : Repo -> Value
jsonEncRepo  val =
   Json.Encode.object
   [ ("name", Json.Encode.string val.name)
   , ("owner", (maybeEncode (Json.Encode.string)) val.owner)
   , ("org", (maybeEncode (Json.Encode.string)) val.org)
   , ("problem", Json.Encode.int val.problem)
   , ("private", Json.Encode.bool val.private)
   , ("only", (maybeEncode (Json.Encode.string)) val.only)
   ]



type alias Problem  =
   { id: Int
   , name: String
   , repo: String
   , difficulty: Int
   , challenge_branches: (List String)
   , answer_branch: String
   , ci_branch: String
   , default_branch: String
   }

jsonDecProblem : Json.Decode.Decoder ( Problem )
jsonDecProblem =
   Json.Decode.succeed (\pid pname prepo pdifficulty pchallenge_branches panswer_branch pci_branch pdefault_branch -> {id = pid, name = pname, repo = prepo, difficulty = pdifficulty, challenge_branches = pchallenge_branches, answer_branch = panswer_branch, ci_branch = pci_branch, default_branch = pdefault_branch})
   |> required "id" (Json.Decode.int)
   |> required "name" (Json.Decode.string)
   |> required "repo" (Json.Decode.string)
   |> required "difficulty" (Json.Decode.int)
   |> required "challenge_branches" (Json.Decode.list (Json.Decode.string))
   |> required "answer_branch" (Json.Decode.string)
   |> required "ci_branch" (Json.Decode.string)
   |> required "default_branch" (Json.Decode.string)

jsonEncProblem : Problem -> Value
jsonEncProblem  val =
   Json.Encode.object
   [ ("id", Json.Encode.int val.id)
   , ("name", Json.Encode.string val.name)
   , ("repo", Json.Encode.string val.repo)
   , ("difficulty", Json.Encode.int val.difficulty)
   , ("challenge_branches", (Json.Encode.list Json.Encode.string) val.challenge_branches)
   , ("answer_branch", Json.Encode.string val.answer_branch)
   , ("ci_branch", Json.Encode.string val.ci_branch)
   , ("default_branch", Json.Encode.string val.default_branch)
   ]



type alias Config  =
   { scoreboard: ScoreBoardConfig
   , problems: (List Problem)
   , teams: (List Team)
   , owners: (List User)
   , image: String
   }

jsonDecConfig : Json.Decode.Decoder ( Config )
jsonDecConfig =
   Json.Decode.succeed (\pscoreboard pproblems pteams powners pimage -> {scoreboard = pscoreboard, problems = pproblems, teams = pteams, owners = powners, image = pimage})
   |> required "scoreboard" (jsonDecScoreBoardConfig)
   |> required "problems" (Json.Decode.list (jsonDecProblem))
   |> required "teams" (Json.Decode.list (jsonDecTeam))
   |> required "owners" (Json.Decode.list (jsonDecUser))
   |> required "image" (Json.Decode.string)

jsonEncConfig : Config -> Value
jsonEncConfig  val =
   Json.Encode.object
   [ ("scoreboard", jsonEncScoreBoardConfig val.scoreboard)
   , ("problems", (Json.Encode.list jsonEncProblem) val.problems)
   , ("teams", (Json.Encode.list jsonEncTeam) val.teams)
   , ("owners", (Json.Encode.list jsonEncUser) val.owners)
   , ("image", Json.Encode.string val.image)
   ]



type alias ScoreBoardConfig  =
   { interval: Float
   , start_time: (Maybe Int)
   , end_time: (Maybe Int)
   , zone: (Maybe String)
   , scoring: (Maybe Bool)
   }

jsonDecScoreBoardConfig : Json.Decode.Decoder ( ScoreBoardConfig )
jsonDecScoreBoardConfig =
   Json.Decode.succeed (\pinterval pstart_time pend_time pzone pscoring -> {interval = pinterval, start_time = pstart_time, end_time = pend_time, zone = pzone, scoring = pscoring})
   |> required "interval" (Json.Decode.float)
   |> fnullable "start_time" (Json.Decode.int)
   |> fnullable "end_time" (Json.Decode.int)
   |> fnullable "zone" (Json.Decode.string)
   |> fnullable "scoring" (Json.Decode.bool)

jsonEncScoreBoardConfig : ScoreBoardConfig -> Value
jsonEncScoreBoardConfig  val =
   Json.Encode.object
   [ ("interval", Json.Encode.float val.interval)
   , ("start_time", (maybeEncode (Json.Encode.int)) val.start_time)
   , ("end_time", (maybeEncode (Json.Encode.int)) val.end_time)
   , ("zone", (maybeEncode (Json.Encode.string)) val.zone)
   , ("scoring", (maybeEncode (Json.Encode.bool)) val.scoring)
   ]



type alias Score  =
   { team: String
   , point: Int
   , stats: (List Status)
   , links: (List Link)
   }

jsonDecScore : Json.Decode.Decoder ( Score )
jsonDecScore =
   Json.Decode.succeed (\pteam ppoint pstats plinks -> {team = pteam, point = ppoint, stats = pstats, links = plinks})
   |> required "team" (Json.Decode.string)
   |> required "point" (Json.Decode.int)
   |> required "stats" (Json.Decode.list (jsonDecStatus))
   |> required "links" (Json.Decode.list (jsonDecLink))

jsonEncScore : Score -> Value
jsonEncScore  val =
   Json.Encode.object
   [ ("team", Json.Encode.string val.team)
   , ("point", Json.Encode.int val.point)
   , ("stats", (Json.Encode.list jsonEncStatus) val.stats)
   , ("links", (Json.Encode.list jsonEncLink) val.links)
   ]



type alias Status  =
   { problem_id: Int
   , correct: Bool
   , pending: Bool
   , corrected_at: (Maybe Int)
   , answerer: (Maybe String)
   }

jsonDecStatus : Json.Decode.Decoder ( Status )
jsonDecStatus =
   Json.Decode.succeed (\pproblem_id pcorrect ppending pcorrected_at panswerer -> {problem_id = pproblem_id, correct = pcorrect, pending = ppending, corrected_at = pcorrected_at, answerer = panswerer})
   |> required "problem_id" (Json.Decode.int)
   |> required "correct" (Json.Decode.bool)
   |> required "pending" (Json.Decode.bool)
   |> fnullable "corrected_at" (Json.Decode.int)
   |> fnullable "answerer" (Json.Decode.string)

jsonEncStatus : Status -> Value
jsonEncStatus  val =
   Json.Encode.object
   [ ("problem_id", Json.Encode.int val.problem_id)
   , ("correct", Json.Encode.bool val.correct)
   , ("pending", Json.Encode.bool val.pending)
   , ("corrected_at", (maybeEncode (Json.Encode.int)) val.corrected_at)
   , ("answerer", (maybeEncode (Json.Encode.string)) val.answerer)
   ]



type alias Link  =
   { problem_id: Int
   , url: String
   }

jsonDecLink : Json.Decode.Decoder ( Link )
jsonDecLink =
   Json.Decode.succeed (\pproblem_id purl -> {problem_id = pproblem_id, url = purl})
   |> required "problem_id" (Json.Decode.int)
   |> required "url" (Json.Decode.string)

jsonEncLink : Link -> Value
jsonEncLink  val =
   Json.Encode.object
   [ ("problem_id", Json.Encode.int val.problem_id)
   , ("url", Json.Encode.string val.url)
   ]


getApiTeams : (Result Http.Error  ((List Team))  -> msg) -> Cmd msg
getApiTeams toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "api"
                    , "teams"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecTeam))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getApiProblems : (Result Http.Error  ((List Problem))  -> msg) -> Cmd msg
getApiProblems toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "api"
                    , "problems"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecProblem))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getApiScores : (Result Http.Error  ((List Score))  -> msg) -> Cmd msg
getApiScores toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "api"
                    , "scores"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecScore))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getApiScoresByTeam : String -> (Result Http.Error  ((List Score))  -> msg) -> Cmd msg
getApiScoresByTeam capture_team toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "api"
                    , "scores"
                    , (capture_team)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecScore))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getApiScoresByTeamByPlayer : String -> String -> (Result Http.Error  ((List Score))  -> msg) -> Cmd msg
getApiScoresByTeamByPlayer capture_team capture_player toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "api"
                    , "scores"
                    , (capture_team)
                    , (capture_player)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecScore))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
