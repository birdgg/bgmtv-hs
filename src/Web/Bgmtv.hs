-- | BGM.tv API client for Haskell
--
-- @
-- client <- newBgmtvClient (defaultConfig "my-app\/1.0")
-- result <- client.searchAnime "葬送的芙莉蓮"
-- @
--
-- Use 'newBgmtvClientWith' to provide a custom 'Network.HTTP.Client.Manager'.
module Web.Bgmtv
  ( -- * Configuration
    BgmtvConfig (..)
  , defaultConfig

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

    -- * Types
  , module Web.Bgmtv.Types

    -- * API
  , bgmtvBaseUrl

    -- * Errors
  , BgmtvError (..)
  )
where

import Web.Bgmtv.API (bgmtvBaseUrl)
import Web.Bgmtv.Client
import Web.Bgmtv.Types
