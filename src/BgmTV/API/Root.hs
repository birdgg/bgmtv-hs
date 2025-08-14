module BgmTV.API.Root where

import BgmTV.API.Calendar
import BgmTV.Types.Subject
import Data.Proxy
import Servant.API
import Servant.Client

type V0 = "v0"

type BgmTVAPI = CalendarAPI

bgmTVAPI :: Proxy BgmTVAPI
bgmTVAPI = Proxy

getCalendar :: ClientM [Calendar]
getCalendar = client bgmTVAPI
