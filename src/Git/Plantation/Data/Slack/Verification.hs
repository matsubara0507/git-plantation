-- | ref: https://api.slack.com/authentication/verifying-requests-from-slack

module Git.Plantation.Data.Slack.Verification
    ( SigningSecret
    , RequestTimestamp
    , SignatureHeader
    , encodeSignature
    , convertSignatureHeader
    ) where

import           RIO
import qualified RIO.ByteString          as B
import qualified RIO.Text                as Text
import qualified RIO.Text.Partial        as Text

import           Crypto.Hash             (Digest, SHA256, digestFromByteString)
import           Crypto.MAC.HMAC         (HMAC (..), hmac)
import           Data.ByteArray.Encoding (Base (..), convertFromBase)

newtype SigningSecret = SigningSecret ByteString deriving (IsString)

type RequestTimestamp = Text

type SignatureHeader = Text

encodeSignature :: SigningSecret -> RequestTimestamp -> ByteString -> Digest SHA256
encodeSignature (SigningSecret secret) ts body =
  hmacGetDigest $ hmac secret basestr
  where
    basestr = B.intercalate ":" [Text.encodeUtf8 version, Text.encodeUtf8 ts, body]

convertSignatureHeader :: SignatureHeader -> Maybe (Digest SHA256)
convertSignatureHeader sign = either (const Nothing) digestFromByteString bs
  where
    (_, sign') = Text.breakOnEnd (version <> "=") sign
    bs = convertFromBase Base16 (Text.encodeUtf8 sign') :: Either String ByteString

version :: Text
version = "v0"
