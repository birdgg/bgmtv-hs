{-# LANGUAGE OverloadedStrings #-}

module BgmTV where

import BgmTV.API.Root
import Data.Text
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import Servant.Client.Core (Request, addHeader)
import Servant.Client.Internal.HttpClient (ClientMiddleware)

bgmTVBaseUrl :: BaseUrl
bgmTVBaseUrl = BaseUrl Https "api.bgm.tv" 443 ""

newtype BgmTVClientEnv = BgmTVClientEnv
  { unBgmTVClientEnv :: ClientEnv
  }

addUserAgent :: Text -> Request -> Request
addUserAgent = addHeader "User-Agent"

bgmMiddleware :: Text -> ClientMiddleware
bgmMiddleware userAgent oapp = oapp . addUserAgent userAgent

mkBgmClientEnv :: Text -> Client.Manager -> BgmTVClientEnv
mkBgmClientEnv userAgent manager = BgmTVClientEnv $ defaultClientEnv{middleware = bgmMiddleware userAgent}
 where
  defaultClientEnv = mkClientEnv manager bgmTVBaseUrl

runBgmClientM :: ClientM a -> BgmTVClientEnv -> IO (Either ClientError a)
runBgmClientM action env = runClientM action (unBgmTVClientEnv env)
