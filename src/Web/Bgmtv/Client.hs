-- | BGM.tv API client using servant-client
module Web.Bgmtv.Client
  ( -- * Configuration
    BgmtvConfig (..)
  , defaultConfig

    -- * Client Record
  , BgmtvClient
  , newBgmtvClient
  , newBgmtvClientWith

  )
where

import Data.Int (Int64)
import Data.Text (Text)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (BaseUrl, ClientEnv, ClientError, ClientM, mkClientEnv, runClientM)
import Servant.Client.Generic (AsClientT, genericClient)
import Web.Bgmtv.API (BgmtvRoutes, bgmtvBaseUrl)
import qualified Web.Bgmtv.API as API
import Web.Bgmtv.Types.Calendar (CalendarItem)
import Web.Bgmtv.Types.Episode (EpisodesResponse)
import Web.Bgmtv.Types.Id (SubjectId)
import Web.Bgmtv.Types.Search (SearchRequest, SearchResponse)
import Web.Bgmtv.Types.Subject (SubjectDetail)

data BgmtvConfig = BgmtvConfig
  { userAgent :: Text
  , baseUrl :: BaseUrl
  }
  deriving stock (Show, Eq)

-- | Create default configuration with custom user-agent
defaultConfig :: Text -> BgmtvConfig
defaultConfig ua =
  BgmtvConfig
    { userAgent = ua
    , baseUrl = bgmtvBaseUrl
    }

-- | BGM.tv API client with pre-configured settings
--
-- Use 'newBgmtvClient' to create a client, then call methods using record dot syntax:
--
-- @
-- client <- newBgmtvClient (defaultConfig "my-app\/1.0")
-- detail <- client.getSubject (SubjectId 425251)
-- @
data BgmtvClient = BgmtvClient
  { -- | Search subjects with custom request
    searchSubjects :: SearchRequest -> IO (Either ClientError SearchResponse)
  , -- | Get subject details by ID
    getSubject :: SubjectId -> IO (Either ClientError SubjectDetail)
  , -- | Get episodes with manual pagination
    getEpisodes :: SubjectId -> Maybe Int64 -> Maybe Int64 -> IO (Either ClientError EpisodesResponse)
  , -- | Get daily airing schedule (legacy API)
    getCalendar :: IO (Either ClientError [CalendarItem])
  }

-- | Create a new BGM.tv client with auto-managed HTTP connection
newBgmtvClient :: BgmtvConfig -> IO BgmtvClient
newBgmtvClient config = do
  manager <- newManager tlsManagerSettings
  pure $ newBgmtvClientWith manager config

-- | Create a new BGM.tv client with a custom HTTP manager
newBgmtvClientWith :: Manager -> BgmtvConfig -> BgmtvClient
newBgmtvClientWith manager config =
  let run :: ClientM a -> IO (Either ClientError a)
      run = runBgmtvWith manager config
  in BgmtvClient
      { searchSubjects = \req ->
          run (API.searchSubjects client' config.userAgent req)
      , getSubject = \subjectId ->
          run (API.getSubject client' subjectId config.userAgent)
      , getEpisodes = \subjectId limit offset ->
          run (API.getEpisodes client' config.userAgent subjectId limit offset)
      , getCalendar =
          run (API.getCalendar client' config.userAgent)
      }

-- | Generated client functions record (internal)
client' :: BgmtvRoutes (AsClientT ClientM)
client' = genericClient

-- | Create a client environment
mkEnv :: Manager -> BgmtvConfig -> ClientEnv
mkEnv manager config = mkClientEnv manager config.baseUrl

runBgmtvWith :: Manager -> BgmtvConfig -> ClientM a -> IO (Either ClientError a)
runBgmtvWith manager config action = do
  let env = mkEnv manager config
  runClientM action env


