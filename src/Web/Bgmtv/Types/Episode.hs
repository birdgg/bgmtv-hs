-- | Episode-related types for BGM.tv API
module Web.Bgmtv.Types.Episode
  ( Episode (..),
    EpisodesResponse (..),
  )
where

import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)
import Web.Bgmtv.Types.Enums (EpisodeType)
import Web.Bgmtv.Types.Id (EpisodeId)

-- | Episode item
data Episode = Episode
  { -- | 剧集 ID
    id :: EpisodeId,
    -- | 剧集类型（本篇、SP、OP、ED）
    episodeType :: EpisodeType,
    -- | 原始标题
    name :: Text,
    -- | 中文标题
    nameCn :: Text,
    -- | 排序序号
    sort :: Double,
    -- | 集数编号（可能为空）
    ep :: Maybe Double,
    -- | 放送日期（未播出时为空）
    airdate :: Maybe Day
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
      <*> o .:? "airdate"

-- | Episodes API response from GET /v0/episodes
data EpisodesResponse = EpisodesResponse
  { -- | 剧集列表
    data_ :: [Episode],
    -- | 总剧集数
    total :: Int64,
    -- | 本次返回的最大条目数
    limit :: Int64,
    -- | 结果偏移量
    offset :: Int64
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON EpisodesResponse where
  parseJSON = withObject "EpisodesResponse" $ \o ->
    EpisodesResponse
      <$> o .: "data"
      <*> o .: "total"
      <*> o .: "limit"
      <*> o .: "offset"
