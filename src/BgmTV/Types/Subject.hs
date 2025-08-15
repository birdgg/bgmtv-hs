{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

data SubjectBase = SubjectBase
  { id :: Int
  , stype :: SubjectType
  , name :: Text
  , nameCn :: Text
  , summary :: Text
  , images :: SubjectImages
  , rating :: Maybe SubjectRating
  , rank :: Maybe Int
  }
  deriving (Show, Generic)

instance FromJSON SubjectBase where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "stype" -> "type"
            "nameCn" -> "name_cn"
            other -> other
        }

data Subject = Subject
  { base :: SubjectBase
  , series :: Bool
  , nsfw :: Bool
  , locked :: Bool
  }
  deriving (Show, Generic)

instance FromJSON Subject where
  parseJSON = withObject "Subject" $ \o -> do
    baseSubject <- parseJSON (Object o)
    series <- o .: "series"
    nsfw <- o .: "nsfw"
    locked <- o .: "locked"
    pure $ Subject baseSubject series nsfw locked

data SubjectSmall = SubjectSmall
  { base :: SubjectBase
  , url :: Text
  , airWeekday :: Int
  }
  deriving (Show, Generic)

instance FromJSON SubjectSmall where
  parseJSON = withObject "SubjectSmall" $ \o -> do
    baseSubject <- parseJSON (Object o)
    url <- o .: "url"
    airWeekday <- o .: "air_weekday"
    pure $ SubjectSmall baseSubject url airWeekday

data Weekday = Weekday {en :: Text, cn :: Text, ja :: Text} deriving (Show, Generic)

instance ToJSON Weekday
instance FromJSON Weekday

type Calendar = [CalendarDay]

data CalendarDay = CalendarDay
  { weekday :: Weekday
  , items :: [SubjectSmall]
  }
  deriving (Show, Generic)

instance FromJSON CalendarDay

data SubjectQuery = SubjectQuery
  { keyword :: Text
  , sort :: Maybe SubjectSort
  , filter :: Maybe SubjectFilter
  }
  deriving (Generic, Show)

instance ToJSON SubjectQuery

data SubjectSort = Match | Heat | Rank | Score deriving (Show, Eq)

instance ToJSON SubjectSort where
  toJSON Match = String "match"
  toJSON Heat = String "heat"
  toJSON Rank = String "rank"
  toJSON Score = String "score"

data SubjectFilter = SubjectFilter
  { stype :: Maybe [SubjectType]
  , metaTags :: Maybe [Text]
  , tags :: Maybe [Text]
  , airDate :: Maybe [Text]
  -- ^ example: List [ ">=2020-07-01", "<2020-10-01" ]
  , rating :: Maybe [Text]
  -- ^ example: List [ ">=6", "<8" ]
  , rank :: Maybe [Text]
  -- ^ example: List [ ">10", "<=18" ]
  , nsfw :: Maybe Bool
  }
  deriving (Generic, Show)

instance ToJSON SubjectFilter where
  toJSON SubjectFilter{..} =
    object
      [ "type" .= stype
      , "meta_tags" .= metaTags
      , "tags" .= tags
      , "air_date" .= airDate
      , "rating" .= rating
      , "rank" .= rank
      , "nsfw" .= nsfw
      ]
