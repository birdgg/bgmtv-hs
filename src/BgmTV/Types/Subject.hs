module BgmTV.Types.Subject where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data SubjectType = Book | Anime | Music | Game | Other deriving (Show, Eq)

instance ToJSON SubjectType where
  toJSON Book = Number 1
  toJSON Anime = Number 2
  toJSON Music = Number 3
  toJSON Game = Number 4
  toJSON Other = Number 6

instance FromJSON SubjectType where
  parseJSON (Number 1) = pure Book
  parseJSON (Number 2) = pure Anime
  parseJSON (Number 3) = pure Music
  parseJSON (Number 4) = pure Game
  parseJSON (Number 6) = pure Other
  parseJSON _ = fail "Invalid SubjectType"

data SubjectImages = SubjectImages
  { large :: Text
  , common :: Text
  , medium :: Text
  , small :: Text
  , grid :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON SubjectImages
instance FromJSON SubjectImages

data SubjectRating = SubjectRating
  { total :: Int
  , score :: Double
  }
  deriving (Show, Generic)

instance ToJSON SubjectRating
instance FromJSON SubjectRating

data Subject = Subject
  { id :: Int
  , url :: Text
  , stype :: [SubjectType]
  , name :: Text
  , nameCn :: Text
  , summary :: Text
  , airDate :: Text
  , airWeekday :: Int
  , images :: SubjectImages
  , eps :: Int
  , epsCount :: Int
  , rating :: SubjectRating
  , rank :: Int
  }
  deriving (Show, Generic)

instance ToJSON Subject where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = \case
            "stype" -> "type"
            "airDate" -> "air_date"
            "nameCn" -> "name_cn"
            "airWeekday" -> "air_weekday"
            "epsCount" -> "eps_count"
            other -> other
        }

instance FromJSON Subject where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "type" -> "stype"
            "air_date" -> "airDate"
            "name_cn" -> "nameCn"
            "air_weekday" -> "airWeekday"
            "eps_count" -> "epsCount"
            other -> other
        }

data Weekday = Weekday {en :: Text, cn :: Text, ja :: Text} deriving (Show, Generic)

instance ToJSON Weekday
instance FromJSON Weekday

data Calendar = Calendar {weekday :: Weekday, items :: [Subject]} deriving (Show, Generic)

instance ToJSON Calendar
instance FromJSON Calendar
