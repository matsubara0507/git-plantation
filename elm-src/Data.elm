module Data exposing (Model, Msg(..))

import Browser as Browser
import Browser.Navigation as Nav
import Generated.API as API exposing (..)
import Http
import RemoteData exposing (RemoteData(..))
import Time exposing (Posix)
import Url exposing (Url)


type alias Model =
    { key : Nav.Key
    , url : Url
    , reload : Bool
    , problems : List API.Problem
    , teams : List API.Team
    , scores : RemoteData String (List API.Score)
    , interval : Float
    }


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | CheckReload Bool
    | Reload
    | Tick Posix
    | FetchScores (Result Http.Error (List API.Score))
