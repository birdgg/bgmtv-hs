-- | Subject-related types for BGM.tv API
module Web.Bgmtv.Types.Subject
  ( Subject (..),
    SubjectDetail (..),
    SubjectImages (..),
    LegacySubject (..),
    Rating (..),
    RatingCount (..),
    SubjectCollection (..),
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)
import Web.Bgmtv.Types.Enums (Platform, SubjectType)
import Web.Bgmtv.Types.Id (SubjectId)

-- | Parse an optional date field, treating empty strings and "0000-00-00" as Nothing
parseOptionalDay :: Object -> Key -> Parser (Maybe Day)
parseOptionalDay o key = do
  mval <- o .:? key :: Parser (Maybe Text)
  case mval of
    Nothing -> pure Nothing
    Just t
      | T.null t || t == "0000-00-00" -> pure Nothing
      | otherwise -> Just <$> (o .: key :: Parser Day)

-- | Subject images from BGM.tv API
data SubjectImages = SubjectImages
  { -- | 小尺寸封面图 URL
    small :: Text,
    -- | 网格尺寸封面图 URL
    grid :: Text,
    -- | 大尺寸封面图 URL
    large :: Text,
    -- | 中尺寸封面图 URL
    medium :: Text,
    -- | 通用尺寸封面图 URL
    common :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Subject item in search results
data Subject = Subject
  { -- | 条目 ID
    id :: SubjectId,
    -- | 原始名称（通常为日文）
    name :: Text,
    -- | 中文名称
    nameCn :: Text,
    -- | 放送/发售日期（可能为空）
    date :: Maybe Day,
    -- | 播放平台
    platform :: Platform,
    -- | 各尺寸封面图
    images :: SubjectImages,
    -- | 默认封面图 URL（可能为空）
    image :: Maybe Text,
    -- | 总集数
    eps :: Int64
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Subject where
  parseJSON = withObject "Subject" $ \o ->
    Subject
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "name_cn"
      <*> parseOptionalDay o "date"
      <*> o .: "platform"
      <*> o .: "images"
      <*> o .:? "image"
      <*> o .: "eps"

-- | Detailed subject information from GET /v0/subjects/{id}
data SubjectDetail = SubjectDetail
  { -- | 条目 ID
    id :: SubjectId,
    -- | 条目类型
    subjectType :: SubjectType,
    -- | 原始名称（通常为日文）
    name :: Text,
    -- | 中文名称
    nameCn :: Text,
    -- | 条目简介
    summary :: Text,
    -- | 是否为 NSFW 内容
    nsfw :: Bool,
    -- | 条目是否被锁定
    locked :: Bool,
    -- | 放送/发售日期（可能为空）
    date :: Maybe Day,
    -- | 播放平台
    platform :: Platform,
    -- | 各尺寸封面图
    images :: SubjectImages,
    -- | 卷数（书籍类型适用）
    volumes :: Int64,
    -- | 已更新集数
    eps :: Int64,
    -- | 总集数
    totalEpisodes :: Int64,
    -- | 元标签列表
    metaTags :: [Text],
    -- | 是否为系列作品
    series :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SubjectDetail where
  parseJSON = withObject "SubjectDetail" $ \o ->
    SubjectDetail
      <$> o .: "id"
      <*> o .: "type"
      <*> o .: "name"
      <*> o .: "name_cn"
      <*> o .: "summary"
      <*> o .: "nsfw"
      <*> o .: "locked"
      <*> parseOptionalDay o "date"
      <*> o .: "platform"
      <*> o .: "images"
      <*> o .: "volumes"
      <*> o .: "eps"
      <*> o .: "total_episodes"
      <*> o .:? "meta_tags" .!= []
      <*> o .: "series"

-- | Legacy subject type returned by /calendar endpoint
data LegacySubject = LegacySubject
  { -- | 条目 ID
    id :: SubjectId,
    -- | 条目页面 URL
    url :: Text,
    -- | 条目类型
    subjectType :: SubjectType,
    -- | 原始名称
    name :: Text,
    -- | 中文名称
    nameCn :: Text,
    -- | 剧情简介
    summary :: Text,
    -- | 放送开始日期
    airDate :: Maybe Day,
    -- | 放送星期
    airWeekday :: Maybe Int,
    -- | 封面图
    images :: Maybe SubjectImages,
    -- | 话数
    eps :: Maybe Int64,
    -- | 评分信息
    rating :: Maybe Rating,
    -- | 排名
    rank :: Maybe Int64,
    -- | 收藏统计
    collection :: Maybe SubjectCollection
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON LegacySubject where
  parseJSON = withObject "LegacySubject" $ \o ->
    LegacySubject
      <$> o .: "id"
      <*> o .: "url"
      <*> o .: "type"
      <*> o .: "name"
      <*> o .: "name_cn"
      <*> o .: "summary"
      <*> parseOptionalDay o "air_date"
      <*> o .:? "air_weekday"
      <*> o .:? "images"
      <*> o .:? "eps"
      <*> o .:? "rating"
      <*> o .:? "rank"
      <*> o .:? "collection"

-- | Rating information for a subject
data Rating = Rating
  { -- | 总评分人数
    total :: Int64,
    -- | 各分值评分人数
    count :: RatingCount,
    -- | 综合评分
    score :: Double
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Per-score rating counts (1-10)
data RatingCount = RatingCount
  { -- | 评分 1 的人数
    r1 :: Int64,
    -- | 评分 2 的人数
    r2 :: Int64,
    -- | 评分 3 的人数
    r3 :: Int64,
    -- | 评分 4 的人数
    r4 :: Int64,
    -- | 评分 5 的人数
    r5 :: Int64,
    -- | 评分 6 的人数
    r6 :: Int64,
    -- | 评分 7 的人数
    r7 :: Int64,
    -- | 评分 8 的人数
    r8 :: Int64,
    -- | 评分 9 的人数
    r9 :: Int64,
    -- | 评分 10 的人数
    r10 :: Int64
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON RatingCount where
  parseJSON = withObject "RatingCount" $ \o ->
    RatingCount
      <$> o .: "1"
      <*> o .: "2"
      <*> o .: "3"
      <*> o .: "4"
      <*> o .: "5"
      <*> o .: "6"
      <*> o .: "7"
      <*> o .: "8"
      <*> o .: "9"
      <*> o .: "10"

instance ToJSON RatingCount where
  toJSON rc =
    object
      [ "1" .= rc.r1,
        "2" .= rc.r2,
        "3" .= rc.r3,
        "4" .= rc.r4,
        "5" .= rc.r5,
        "6" .= rc.r6,
        "7" .= rc.r7,
        "8" .= rc.r8,
        "9" .= rc.r9,
        "10" .= rc.r10
      ]

-- | Collection statistics for a subject
data SubjectCollection = SubjectCollection
  { -- | 想看
    wish :: Int64,
    -- | 看过
    collect :: Int64,
    -- | 在看
    doing :: Int64,
    -- | 搁置
    onHold :: Int64,
    -- | 抛弃
    dropped :: Int64
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SubjectCollection where
  parseJSON = withObject "SubjectCollection" $ \o ->
    SubjectCollection
      <$> o .: "wish"
      <*> o .: "collect"
      <*> o .: "doing"
      <*> o .: "on_hold"
      <*> o .: "dropped"

instance ToJSON SubjectCollection where
  toJSON sc =
    object
      [ "wish" .= sc.wish,
        "collect" .= sc.collect,
        "doing" .= sc.doing,
        "on_hold" .= sc.onHold,
        "dropped" .= sc.dropped
      ]
