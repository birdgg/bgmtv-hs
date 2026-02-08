-- | BGM.tv API client for Haskell
--
-- @
-- client <- newBgmtvClient (defaultConfig "my-app\/1.0")
-- detail <- client.getSubject (SubjectId 425251)
-- @
--
-- Use 'newBgmtvClientWith' to provide a custom 'Network.HTTP.Client.Manager'.
module Web.Bgmtv
  ( -- * Configuration
    BgmtvConfig (..)
  , defaultConfig

    -- * Client Record
  , BgmtvClient
  , newBgmtvClient
  , newBgmtvClientWith

    -- * Types

    -- ** ID Types
  , module Web.Bgmtv.Types.Id

    -- ** Enums
  , module Web.Bgmtv.Types.Enums

    -- ** Search
  , module Web.Bgmtv.Types.Search

    -- ** Subject
  , module Web.Bgmtv.Types.Subject

    -- ** Episode
  , module Web.Bgmtv.Types.Episode

    -- ** Calendar
  , module Web.Bgmtv.Types.Calendar

    -- ** Error
  , module Web.Bgmtv.Types.Error

    -- * API
  , bgmtvBaseUrl
  )
where

import Web.Bgmtv.API (bgmtvBaseUrl)
import Web.Bgmtv.Client
import Web.Bgmtv.Types.Calendar
import Web.Bgmtv.Types.Error
import Web.Bgmtv.Types.Enums
import Web.Bgmtv.Types.Episode
import Web.Bgmtv.Types.Id
import Web.Bgmtv.Types.Search
import Web.Bgmtv.Types.Subject
