{-# LANGUAGE OverloadedStrings #-}

module BgmTV (
  mkBgmClientEnv,
  module BgmTV.API,
  module BgmTV.Types.Subject
) where

import Data.Text
import Network.HTTP.Client qualified as Client
import Servant.Client
import Servant.Client.Core (Request, addHeader)
import Servant.Client.Internal.HttpClient (ClientMiddleware)
import BgmTV.API
import BgmTV.Types.Subject

bgmTVBaseUrl :: BaseUrl
bgmTVBaseUrl = BaseUrl Https "api.bgm.tv" 443 ""

addUserAgent :: Text -> Request -> Request
addUserAgent = addHeader "User-Agent"

bgmMiddleware :: Text -> ClientMiddleware
bgmMiddleware userAgent oapp = oapp . addUserAgent userAgent

mkBgmClientEnv :: Text -> Client.Manager -> ClientEnv
mkBgmClientEnv userAgent manager = defaultClientEnv{middleware = bgmMiddleware userAgent}
 where
  defaultClientEnv = mkClientEnv manager bgmTVBaseUrl
