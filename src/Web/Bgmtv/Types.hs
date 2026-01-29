-- | Types for BGM.tv API
module Web.Bgmtv.Types
  ( -- * Enums
    SubjectType (..)
  , EpisodeType (..)

    -- * Search
  , SearchRequest (..)
  , SearchFilter (..)
  , SearchResponse (..)

    -- * Subject
  , Subject (..)
  , SubjectDetail (..)
  , SubjectImages (..)

    -- * Episode
  , Episode (..)
  , EpisodesResponse (..)
  )
where

import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | BGM.tv subject type
data SubjectType
  = Book   -- ^ 书籍 (type=1)
  | Anime  -- ^ 动画 (type=2)
  | Music  -- ^ 音乐 (type=3)
  | Game   -- ^ 游戏 (type=4)
  | Real   -- ^ 三次元 (type=6)
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

-- | Search request body for POST /v0/search/subjects
data SearchRequest = SearchRequest
  { keyword :: Text                  -- ^ 搜索关键词
  , filter_ :: Maybe SearchFilter    -- ^ 可选的搜索过滤条件
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SearchRequest where
  toJSON req =
    object $
      ["keyword" .= req.keyword]
        <> maybe [] (\f -> ["filter" .= f]) req.filter_

-- | Search filter options
data SearchFilter = SearchFilter
  { subjectType :: Maybe [SubjectType]  -- ^ 条目类型过滤，可指定多个类型
  , metaTags :: Maybe [Text]            -- ^ 元标签过滤，如 \"TV\"、\"剧场版\" 等
  , airDate :: Maybe [Text]             -- ^ 放送日期过滤，格式如 \">=2020-01-01\"
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SearchFilter where
  toJSON f =
    object $
      catMaybes
        [ ("type" .=) <$> f.subjectType
        , ("meta_tags" .=) <$> f.metaTags
        , ("air_date" .=) <$> f.airDate
        ]
    where
      catMaybes = foldr (\x acc -> maybe acc (: acc) x) []

-- | Search response from POST /v0/search/subjects
data SearchResponse = SearchResponse
  { total :: Int64       -- ^ 匹配的条目总数
  , limit :: Int64       -- ^ 本次返回的最大条目数
  , offset :: Int64      -- ^ 结果偏移量
  , data_ :: [Subject]   -- ^ 搜索结果列表
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SearchResponse where
  parseJSON = withObject "SearchResponse" $ \o ->
    SearchResponse
      <$> o .: "total"
      <*> o .: "limit"
      <*> o .: "offset"
      <*> o .: "data"

-- | Subject images from BGM.tv API
data SubjectImages = SubjectImages
  { small :: Text    -- ^ 小尺寸封面图 URL
  , grid :: Text     -- ^ 网格尺寸封面图 URL
  , large :: Text    -- ^ 大尺寸封面图 URL
  , medium :: Text   -- ^ 中尺寸封面图 URL
  , common :: Text   -- ^ 通用尺寸封面图 URL
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Subject item in search results
data Subject = Subject
  { id :: Int64             -- ^ 条目 ID
  , name :: Text            -- ^ 原始名称（通常为日文）
  , nameCn :: Text          -- ^ 中文名称
  , date :: Text            -- ^ 放送/发售日期
  , platform :: Text        -- ^ 平台（如 \"TV\"、\"WEB\" 等）
  , images :: SubjectImages -- ^ 各尺寸封面图
  , image :: Text           -- ^ 默认封面图 URL
  , eps :: Int64            -- ^ 总集数
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Subject where
  parseJSON = withObject "Subject" $ \o ->
    Subject
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "name_cn"
      <*> o .: "date"
      <*> o .: "platform"
      <*> o .: "images"
      <*> o .: "image"
      <*> o .: "eps"

-- | Detailed subject information from GET /v0/subjects/{id}
data SubjectDetail = SubjectDetail
  { id :: Int64               -- ^ 条目 ID
  , subjectType :: SubjectType -- ^ 条目类型
  , name :: Text              -- ^ 原始名称（通常为日文）
  , nameCn :: Text            -- ^ 中文名称
  , summary :: Text           -- ^ 条目简介
  , nsfw :: Bool              -- ^ 是否为 NSFW 内容
  , locked :: Bool            -- ^ 条目是否被锁定
  , date :: Maybe Text        -- ^ 放送/发售日期（可能为空）
  , platform :: Maybe Text    -- ^ 平台（可能为空）
  , images :: SubjectImages   -- ^ 各尺寸封面图
  , volumes :: Int64          -- ^ 卷数（书籍类型适用）
  , eps :: Int64              -- ^ 已更新集数
  , totalEpisodes :: Int64    -- ^ 总集数
  , metaTags :: [Text]        -- ^ 元标签列表
  , series :: Bool            -- ^ 是否为系列作品
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
      <*> o .:? "date"
      <*> o .:? "platform"
      <*> o .: "images"
      <*> o .: "volumes"
      <*> o .: "eps"
      <*> o .: "total_episodes"
      <*> o .:? "meta_tags" .!= []
      <*> o .: "series"

-- | Episode item
data Episode = Episode
  { id :: Int64               -- ^ 剧集 ID
  , episodeType :: EpisodeType -- ^ 剧集类型（本篇、SP、OP、ED）
  , name :: Text              -- ^ 原始标题
  , nameCn :: Text            -- ^ 中文标题
  , sort :: Double            -- ^ 排序序号
  , ep :: Maybe Double        -- ^ 集数编号（可能为空）
  , airdate :: Text           -- ^ 放送日期
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Episode where
  parseJSON = withObject "Episode" $ \o ->
    Episode
      <$> o .: "id"
      <*> o .: "type"
      <*> o .: "name"
      <*> o .: "name_cn"
      <*> o .: "sort"
      <*> o .:? "ep"
      <*> o .: "airdate"

-- | Episodes API response from GET /v0/episodes
data EpisodesResponse = EpisodesResponse
  { data_ :: [Episode]  -- ^ 剧集列表
  , total :: Int64      -- ^ 总剧集数
  , limit :: Int64      -- ^ 本次返回的最大条目数
  , offset :: Int64     -- ^ 结果偏移量
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON EpisodesResponse where
  parseJSON = withObject "EpisodesResponse" $ \o ->
    EpisodesResponse
      <$> o .: "data"
      <*> o .: "total"
      <*> o .: "limit"
      <*> o .: "offset"
