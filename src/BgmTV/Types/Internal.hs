module BgmTV.Types.Internal (
  Pagination,
  Date,
) where

import Data.Aeson
import Data.Text
import GHC.Generics

data Pagination a = Pagination
  { total :: Int
  , limit :: Int
  , offset :: Int
  , pdata :: [a]
  }
  deriving (Show, Generic)

instance (FromJSON a) => FromJSON (Pagination a)

-- | format "YYYY-MM-DD"
type Date = Text
