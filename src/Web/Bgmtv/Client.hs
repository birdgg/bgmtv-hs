-- | BGM.tv API client using servant-client
module Web.Bgmtv.Client
  ( -- * Configuration
    BgmtvConfig (..)
  , defaultConfig

    -- * Error Types
  , BgmtvError (..)

    -- * Client Record
  , BgmtvClient (..)
  , newBgmtvClient
  , newBgmtvClientWith

    -- * Running Requests
  , runBgmtv
  , runBgmtvWith

    -- * Low-level ClientM Functions
  , searchSubjectsM
  , getSubjectM
  , getEpisodesM
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
import Web.Bgmtv.Types

-- | Configuration for BGM.tv client
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

-- | Errors that can occur during BGM.tv API operations
data BgmtvError
  = -- | Servant client error
    ClientErr ClientError
  deriving stock (Show)

-- | BGM.tv API client with pre-configured settings
--
-- Use 'newBgmtvClient' to create a client, then call methods using record dot syntax:
--
-- @
-- client <- newBgmtvClient (defaultConfig "my-app\/1.0")
-- result <- client.searchAnime "葬送的芙莉蓮"
-- detail <- client.getSubject (SubjectId 425251)
-- @
data BgmtvClient = BgmtvClient
  { -- | Search for Japanese anime by keyword
    searchAnime :: Text -> IO (Either BgmtvError [Subject])
  , -- | Search subjects with custom request
    searchSubjects :: SearchRequest -> IO (Either BgmtvError SearchResponse)
  , -- | Get subject details by ID
    getSubject :: SubjectId -> IO (Either BgmtvError SubjectDetail)
  , -- | Get all episodes with automatic pagination
    getAllEpisodes :: SubjectId -> IO (Either BgmtvError [Episode])
  , -- | Get episodes with manual pagination
    getEpisodes :: SubjectId -> Maybe Int64 -> Maybe Int64 -> IO (Either BgmtvError EpisodesResponse)
  }

-- | Create a new BGM.tv client with auto-managed HTTP connection
newBgmtvClient :: BgmtvConfig -> IO BgmtvClient
newBgmtvClient config = do
  manager <- newManager tlsManagerSettings
  pure $ newBgmtvClientWith manager config

-- | Create a new BGM.tv client with a custom HTTP manager
newBgmtvClientWith :: Manager -> BgmtvConfig -> BgmtvClient
newBgmtvClientWith manager config =
  let run :: ClientM a -> IO (Either BgmtvError a)
      run = runBgmtvWith manager config
  in BgmtvClient
      { searchAnime = \keyword -> do
          let req =
                SearchRequest
                  { keyword = keyword
                  , filter_ =
                      Just
                        SearchFilter
                          { subjectType = Just [Anime]
                          , metaTags = Just ["日本"]
                          , airDate = Nothing
                          }
                  }
          result <- run (searchSubjectsM config.userAgent req)
          pure $ fmap (.data_) result
      , searchSubjects = \req ->
          run (searchSubjectsM config.userAgent req)
      , getSubject = \subjectId ->
          run (getSubjectM config.userAgent subjectId)
      , getAllEpisodes = \subjectId ->
          getAllEpisodesLoop manager config subjectId 0 []
      , getEpisodes = \subjectId limit offset ->
          run (getEpisodesM config.userAgent subjectId limit offset)
      }

-- | Generated client functions record (internal)
client' :: BgmtvRoutes (AsClientT ClientM)
client' = genericClient

-- | Create a client environment
mkEnv :: Manager -> BgmtvConfig -> ClientEnv
mkEnv manager config = mkClientEnv manager config.baseUrl

-- | Run a BGM.tv request with default TLS manager
runBgmtv :: BgmtvConfig -> ClientM a -> IO (Either BgmtvError a)
runBgmtv config action = do
  manager <- newManager tlsManagerSettings
  runBgmtvWith manager config action

-- | Run a BGM.tv request with custom manager
runBgmtvWith :: Manager -> BgmtvConfig -> ClientM a -> IO (Either BgmtvError a)
runBgmtvWith manager config action = do
  let env = mkEnv manager config
  result <- runClientM action env
  pure $ case result of
    Left err -> Left (ClientErr err)
    Right a -> Right a

-- * Low-level ClientM Functions

-- | Search subjects with custom request
searchSubjectsM :: Text -> SearchRequest -> ClientM SearchResponse
searchSubjectsM ua req = API.searchSubjects client' ua req

-- | Get subject details by ID
getSubjectM :: Text -> SubjectId -> ClientM SubjectDetail
getSubjectM ua subjectId = API.getSubject client' subjectId ua

-- | Get episodes with pagination
getEpisodesM :: Text -> SubjectId -> Maybe Int64 -> Maybe Int64 -> ClientM EpisodesResponse
getEpisodesM ua subjectId limit offset =
  API.getEpisodes client' ua subjectId limit offset

-- | Internal: fetch all episodes with pagination loop
getAllEpisodesLoop
  :: Manager
  -> BgmtvConfig
  -> SubjectId
  -> Int64
  -> [Episode]
  -> IO (Either BgmtvError [Episode])
getAllEpisodesLoop manager config subjectId offset acc = do
  let pageSize = 100
  result <- runBgmtvWith manager config $
    getEpisodesM config.userAgent subjectId (Just pageSize) (Just offset)
  case result of
    Left err -> pure (Left err)
    Right resp -> do
      let newAcc = acc <> resp.data_
          fetched = offset + fromIntegral (length resp.data_)
      if fetched >= resp.total
        then pure (Right newAcc)
        else getAllEpisodesLoop manager config subjectId fetched newAcc
