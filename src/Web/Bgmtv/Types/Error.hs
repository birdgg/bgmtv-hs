module Web.Bgmtv.Types.Error
  ( BgmtvError (..),
    ErrorDetails (..),
  )
where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data ErrorDetails = ErrorDetails
  { 
    path :: Text,
    method :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data BgmtvError = BgmtvError
  { 
    title :: Text,
    details :: ErrorDetails,
    requestId :: Text,
    description :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON BgmtvError where
  parseJSON = withObject "BgmtvError" $ \o ->
    BgmtvError
      <$> o .: "title"
      <*> o .: "details"
      <*> o .: "request_id"
      <*> o .: "description"

instance ToJSON BgmtvError where
  toJSON e =
    object
      [ "title" .= e.title,
        "details" .= e.details,
        "request_id" .= e.requestId,
        "description" .= e.description
      ]
