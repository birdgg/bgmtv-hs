-- | Type-safe ID wrappers for BGM.tv entities
module Web.Bgmtv.Types.Id
  ( SubjectId (..),
    EpisodeId (..),
  )
where

import Data.Int (Int64)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Servant.API (FromHttpApiData, ToHttpApiData)

newtype SubjectId = SubjectId {unSubjectId :: Int64}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype EpisodeId = EpisodeId {unEpisodeId :: Int64}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)
