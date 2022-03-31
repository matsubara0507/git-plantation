module Git.Plantation.Job.Protocol where

import           RIO
import qualified RIO.ByteString.Lazy     as BL

import qualified Data.Aeson              as JSON
import qualified Data.Binary             as Binary
import qualified Git.Plantation.Data.Job as Job
import qualified Network.WebSockets      as WS

-- | protocol from Server to Client
data Server
  = JobConfigs [Job.Config]
  | Enqueue Job.Id Job.Name
  | SUndefined

instance WS.WebSocketsData Server where
  fromDataMessage (WS.Text   bl _) = WS.fromLazyByteString bl
  fromDataMessage (WS.Binary bl)   = WS.fromLazyByteString bl

  fromLazyByteString lbs = case BL.uncons lbs of
    Just (1, rest) ->
      maybe
        SUndefined
        JobConfigs
        (JSON.decode rest)

    Just (2, rest) ->
      uncurry Enqueue (Binary.decode rest)

    _ ->
      SUndefined

  toLazyByteString p = case p of
    JobConfigs configs ->
      BL.cons 1 (JSON.encode configs)

    Enqueue wid name ->
      BL.cons 2 (Binary.encode (wid, name))

    SUndefined ->
      ""

-- | protocol from Client to Server
data Client
  = JobRunning Job.Id
  | JobSuccess Job.Id
  | JobFailure Job.Id
  | CUndefined

instance WS.WebSocketsData Client where
  fromDataMessage (WS.Text   bl _) = WS.fromLazyByteString bl
  fromDataMessage (WS.Binary bl)   = WS.fromLazyByteString bl

  fromLazyByteString lbs = case BL.uncons lbs of
    Just (1, rest) ->
      JobRunning (Binary.decode rest)

    Just (2, rest) ->
      JobSuccess (Binary.decode rest)

    Just (3, rest) ->
      JobFailure (Binary.decode rest)

    _ ->
      CUndefined

  toLazyByteString p = case p of
    JobRunning wid ->
      BL.cons 1 (Binary.encode wid)

    JobSuccess wid ->
      BL.cons 2 (Binary.encode wid)

    JobFailure wid ->
      BL.cons 3 (Binary.encode wid)

    CUndefined ->
      ""
