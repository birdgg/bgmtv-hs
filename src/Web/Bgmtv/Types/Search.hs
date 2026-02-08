-- | Search-related types for BGM.tv API
module Web.Bgmtv.Types.Search
  ( SearchRequest (..),
    SearchFilter (..),
    SearchResponse (..),
  )
where

import Data.Aeson
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import Web.Bgmtv.Types.Enums (SubjectType)
import Web.Bgmtv.Types.Subject (Subject)

-- | Search request body for POST /v0/search/subjects
data SearchRequest = SearchRequest
  { -- | 搜索关键词
    keyword :: Text,
    -- | 可选的搜索过滤条件
    filter_ :: Maybe SearchFilter
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SearchRequest where
  toJSON req =
    object $
      ["keyword" .= req.keyword]
        <> maybe [] (\f -> ["filter" .= f]) req.filter_

-- | Search filter options
data SearchFilter = SearchFilter
  { -- | 条目类型过滤，可指定多个类型
    subjectType :: Maybe [SubjectType],
    -- | 元标签过滤，如 "TV"、"剧场版" 等
    metaTags :: Maybe [Text],
    -- | 放送日期过滤，格式如 ">=2020-01-01"
    airDate :: Maybe [Text]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SearchFilter where
  toJSON f =
    object $
      catMaybes
        [ ("type" .=) <$> f.subjectType,
          ("meta_tags" .=) <$> f.metaTags,
          ("air_date" .=) <$> f.airDate
        ]

-- | Search response from POST /v0/search/subjects
data SearchResponse = SearchResponse
  { -- | 匹配的条目总数
    total :: Int64,
    -- | 本次返回的最大条目数
    limit :: Int64,
    -- | 结果偏移量
    offset :: Int64,
    -- | 搜索结果列表
    data_ :: [Subject]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SearchResponse where
  parseJSON = withObject "SearchResponse" $ \o ->
    SearchResponse
      <$> o .: "total"
      <*> o .: "limit"
      <*> o .: "offset"
      <*> o .: "data"
