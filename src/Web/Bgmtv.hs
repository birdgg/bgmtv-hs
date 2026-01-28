-- | BGM.tv API client for Haskell
--
-- This module provides a type-safe client for the BGM.tv API.
--
-- == Quick Start
--
-- @
-- import Web.Bgmtv
--
-- main :: IO ()
-- main = do
--   let config = defaultConfig "your-app\/1.0"
--
--   -- Search for anime
--   result <- searchAnime config "葬送的芙莉蓮"
--   case result of
--     Right subjects -> mapM_ print subjects
--     Left err -> print err
-- @
--
-- == Low-level API
--
-- For more control, use the @*M@ functions with 'runBgmtv':
--
-- @
-- result <- runBgmtv config $ do
--   subjects <- searchSubjectsM config.userAgent req
--   detail <- getSubjectM config.userAgent (head subjects.data_).id
--   pure detail
-- @
module Web.Bgmtv
  ( -- * Configuration
    BgmtvConfig (..)
  , defaultConfig

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
