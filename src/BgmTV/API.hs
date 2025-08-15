module BgmTV.API where

import BgmTV.Types.Internal
import BgmTV.Types.Subject
import Data.Proxy
import Servant.API
import Servant.Client

type V0 = "v0"

type CalendarApi = "calendar" :> Get '[JSON] Calendar

type SearchApi = "search" :> ("subject" :> ReqBody '[JSON] SubjectQuery :> Post '[JSON] (Pagination Subject))

type Api = CalendarApi :<|> (V0 :> SearchApi)

api :: Proxy Api
api = Proxy

getCalendar :: ClientM Calendar
searchSubject :: SubjectQuery -> ClientM (Pagination Subject)
getCalendar :<|> searchSubject = client api
