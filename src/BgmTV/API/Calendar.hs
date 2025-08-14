module BgmTV.API.Calendar where

import BgmTV.Types.Subject
import Data.Proxy
import Servant.API

type CalendarAPI = "calendar" :> Get '[JSON] [Calendar]

calendarAPI :: Proxy CalendarAPI
calendarAPI = Proxy
