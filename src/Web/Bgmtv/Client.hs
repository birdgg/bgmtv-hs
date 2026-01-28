-- | BGM.tv API client using servant-client
module Web.Bgmtv.Client
  ( -- * Configuration
    BgmtvConfig (..)
  , defaultConfig

    -- * Error Types
  , BgmtvError (..)

    -- * Running Requests
  , runBgmtv
  , runBgmtvWith

    -- * High-level Functions
  , searchAnime
  , getSubject
  , getAllEpisodes

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
getSubjectM :: Text -> Int64 -> ClientM SubjectDetail
getSubjectM ua subjectId = API.getSubject client' subjectId ua

-- | Get episodes with pagination
getEpisodesM :: Text -> Int64 -> Maybe Int64 -> Maybe Int64 -> ClientM EpisodesResponse
getEpisodesM ua subjectId limit offset =
  API.getEpisodes client' ua subjectId limit offset

-- * High-level Functions

-- | Search for Japanese anime
searchAnime :: BgmtvConfig -> Text -> IO (Either BgmtvError [Subject])
searchAnime config keyword = do
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
  result <- runBgmtv config (searchSubjectsM config.userAgent req)
  pure $ fmap (.data_) result

-- | Get subject details by ID (convenience wrapper)
getSubject :: BgmtvConfig -> Int64 -> IO (Either BgmtvError SubjectDetail)
getSubject config subjectId =
  runBgmtv config (getSubjectM config.userAgent subjectId)

-- | Get all episodes with automatic pagination
getAllEpisodes :: BgmtvConfig -> Int64 -> IO (Either BgmtvError [Episode])
getAllEpisodes config subjectId = do
  manager <- newManager tlsManagerSettings
  getAllEpisodesLoop manager config subjectId 0 []

-- | Internal: fetch all episodes with pagination loop
getAllEpisodesLoop
  :: Manager
  -> BgmtvConfig
  -> Int64
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
