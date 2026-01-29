-- | BGM.tv API definition for servant-client
module Web.Bgmtv.API
  ( -- * API Types
    BgmtvAPI
  , BgmtvRoutes (..)

    -- * Configuration
  , bgmtvBaseUrl
  )
where

import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API
import Servant.Client (BaseUrl (..), Scheme (..))
import Web.Bgmtv.Types

-- | BGM.tv API v0
type BgmtvAPI = NamedRoutes BgmtvRoutes

-- | BGM.tv API routes using NamedRoutes pattern
data BgmtvRoutes mode = BgmtvRoutes
  { -- | 搜索条目，支持关键词和过滤条件
    searchSubjects
      :: mode
        :- "v0"
          :> "search"
          :> "subjects"
          :> Header' '[Required, Strict] "User-Agent" Text
          :> ReqBody '[JSON] SearchRequest
          :> Post '[JSON] SearchResponse
  , -- | 根据 ID 获取条目详细信息
    getSubject
      :: mode
        :- "v0"
          :> "subjects"
          :> Capture "id" Int64
          :> Header' '[Required, Strict] "User-Agent" Text
          :> Get '[JSON] SubjectDetail
  , -- | 获取指定条目的剧集列表，支持分页
    getEpisodes
      :: mode
        :- "v0"
          :> "episodes"
          :> Header' '[Required, Strict] "User-Agent" Text
          :> QueryParam' '[Required, Strict] "subject_id" Int64
          :> QueryParam "limit" Int64
          :> QueryParam "offset" Int64
          :> Get '[JSON] EpisodesResponse
  }
  deriving stock (Generic)

-- | Base URL for BGM.tv API
bgmtvBaseUrl :: BaseUrl
bgmtvBaseUrl = BaseUrl Https "api.bgm.tv" 443 ""
