module Web.Bgmtv.Types.Error
  ( -- * Error Types
    BgmtvError (..)
  , ApiError (..)
  , ErrorDetails (..)

    -- * Type Aliases
  , Response

    -- * Conversion
  , fromClientError
  )
where

import Control.DeepSeq (NFData (..))
import Control.Exception (Exception)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.Client (ClientError (..), ResponseF (..))

data ErrorDetails = ErrorDetails
  { path :: Text
  , method :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Structured error response body from the BGM.tv API
data ApiError = ApiError
  { title :: Text
  , details :: ErrorDetails
  , requestId :: Text
  , description :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

instance FromJSON ApiError where
  parseJSON = withObject "ApiError" $ \o ->
    ApiError
      <$> o .: "title"
      <*> o .: "details"
      <*> o .: "request_id"
      <*> o .: "description"

instance ToJSON ApiError where
  toJSON e =
    object
      [ "title" .= e.title
      , "details" .= e.details
      , "request_id" .= e.requestId
      , "description" .= e.description
      ]

-- | All possible errors from the BGM.tv client
--
-- Two-layer error separation: network\/HTTP errors ('ServantError') are kept
-- distinct from domain-specific API errors ('BgmtvApiError').
data BgmtvError
  = -- | Network\/HTTP layer error (raw servant-client error)
    ServantError ClientError
  | -- | Structured API error response (parsed from JSON)
    BgmtvApiError ApiError
  deriving stock (Show, Generic)

instance NFData BgmtvError where
  rnf (ServantError !_) = ()
  rnf (BgmtvApiError e) = rnf e

instance Exception BgmtvError

-- | Convenience alias for client return types
type Response a = Either BgmtvError a

-- | Convert a servant 'ClientError' to 'BgmtvError'
--
-- Attempts to parse the response body as a structured 'ApiError'.
-- Falls back to wrapping the raw 'ClientError' as 'ServantError'.
fromClientError :: ClientError -> BgmtvError
fromClientError err@(FailureResponse _req resp) =
  case eitherDecode (responseBody resp) of
    Right apiErr -> BgmtvApiError apiErr
    Left _ -> ServantError err
fromClientError err = ServantError err
