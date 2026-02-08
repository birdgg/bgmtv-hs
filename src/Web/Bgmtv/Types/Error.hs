module Web.Bgmtv.Types.Error
  ( BgmtvError (..)
  , ApiError (..)
  , ErrorDetails (..)
  , fromClientError
  )
where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (statusCode)
import Servant.Client (ClientError (..), ResponseF (..))

data ErrorDetails = ErrorDetails
  { path :: Text
  , method :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Structured error response body from the BGM.tv API
data ApiError = ApiError
  { title :: Text
  , details :: ErrorDetails
  , requestId :: Text
  , description :: Text
  }
  deriving stock (Show, Eq, Generic)

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
data BgmtvError
  = -- | Structured API error response (parsed from JSON)
    BgmtvApiError ApiError
  | -- | HTTP error with status code and raw body
    BgmtvHttpError Int Text
  | -- | Response decode failure
    BgmtvDecodeError Text
  | -- | Network\/connection error
    BgmtvConnectionError Text
  deriving stock (Show, Eq)

-- | Convert a servant 'ClientError' to 'BgmtvError'
fromClientError :: ClientError -> BgmtvError
fromClientError (FailureResponse _req resp) =
  case eitherDecode (responseBody resp) of
    Right apiErr -> BgmtvApiError apiErr
    Left _ ->
      BgmtvHttpError
        (statusCode (responseStatusCode resp))
        (decodeBody (responseBody resp))
fromClientError (DecodeFailure msg _resp) =
  BgmtvDecodeError msg
fromClientError (UnsupportedContentType _mediaType _resp) =
  BgmtvDecodeError "Unsupported content type"
fromClientError (InvalidContentTypeHeader _resp) =
  BgmtvDecodeError "Invalid content type header"
fromClientError (ConnectionError ex) =
  BgmtvConnectionError (T.pack (show ex))

decodeBody :: LBS.ByteString -> Text
decodeBody = TE.decodeUtf8Lenient . LBS.toStrict
