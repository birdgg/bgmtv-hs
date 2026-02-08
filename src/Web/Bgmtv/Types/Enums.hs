-- | Enumeration types for BGM.tv API
module Web.Bgmtv.Types.Enums
  ( SubjectType (..),
    EpisodeType (..),
    Platform (..),
  )
where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

-- | BGM.tv subject type
data SubjectType
  = -- | 书籍 (type=1)
    Book
  | -- | 动画 (type=2)
    Anime
  | -- | 音乐 (type=3)
    Music
  | -- | 游戏 (type=4)
    Game
  | -- | 三次元 (type=6)
    Real
  deriving stock (Show, Eq, Generic)

instance ToJSON SubjectType where
  toJSON = \case
    Book -> Number 1
    Anime -> Number 2
    Music -> Number 3
    Game -> Number 4
    Real -> Number 6

instance FromJSON SubjectType where
  parseJSON = withScientific "SubjectType" $ \n ->
    case round n :: Int of
      1 -> pure Book
      2 -> pure Anime
      3 -> pure Music
      4 -> pure Game
      6 -> pure Real
      _ -> fail $ "Unknown SubjectType: " <> show n

-- | Episode type
data EpisodeType
  = -- | 本篇
    Main
  | -- | SP
    Special
  | -- | OP
    Opening
  | -- | ED
    Ending
  deriving stock (Show, Eq, Generic)

instance ToJSON EpisodeType where
  toJSON = \case
    Main -> Number 0
    Special -> Number 1
    Opening -> Number 2
    Ending -> Number 3

instance FromJSON EpisodeType where
  parseJSON = withScientific "EpisodeType" $ \n ->
    case round n :: Int of
      0 -> pure Main
      1 -> pure Special
      2 -> pure Opening
      3 -> pure Ending
      _ -> fail $ "Unknown EpisodeType: " <> show n

-- | Platform type for anime distribution
data Platform
  = -- | TV 放送
    TV
  | -- | 剧场版/电影
    Theatrical
  | -- | OVA/OAD
    OVA
  | -- | 其他平台
    OtherPlatform Text
  deriving stock (Show, Eq, Generic)

instance ToJSON Platform where
  toJSON = \case
    TV -> String "TV"
    Theatrical -> String "剧场版"
    OVA -> String "OVA"
    OtherPlatform t -> String t

instance FromJSON Platform where
  parseJSON = withText "Platform" $ \t ->
    pure $ case t of
      "TV" -> TV
      "剧场版" -> Theatrical
      "OVA" -> OVA
      other -> OtherPlatform other
