-- | Calendar-related types for BGM.tv legacy API
module Web.Bgmtv.Types.Calendar
  ( CalendarItem (..),
    dayOfWeekEn,
    dayOfWeekCn,
    dayOfWeekJa,
  )
where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Calendar (DayOfWeek (..))
import GHC.Generics (Generic)
import Web.Bgmtv.Types.Subject (LegacySubject)

-- | One day's airing schedule from GET /calendar
data CalendarItem = CalendarItem
  { -- | 星期信息
    weekday :: DayOfWeek,
    -- | 当日放送的条目列表
    items :: [LegacySubject]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CalendarItem where
  parseJSON = withObject "CalendarItem" $ \o -> do
    wd <-
      o .: "weekday" >>= withObject "weekday" (\w -> do
        wid <- w .: "id"
        case intToDayOfWeek wid of
          Just d -> pure d
          Nothing -> fail $ "Invalid weekday id: " <> show (wid :: Int))
    is <- o .: "items"
    pure CalendarItem {weekday = wd, items = is}

dayOfWeekEn :: DayOfWeek -> Text
dayOfWeekEn = \case
  Monday -> "Mon"
  Tuesday -> "Tue"
  Wednesday -> "Wed"
  Thursday -> "Thu"
  Friday -> "Fri"
  Saturday -> "Sat"
  Sunday -> "Sun"

dayOfWeekCn :: DayOfWeek -> Text
dayOfWeekCn = \case
  Monday -> "\26143\26399\19968"
  Tuesday -> "\26143\26399\20108"
  Wednesday -> "\26143\26399\19977"
  Thursday -> "\26143\26399\22235"
  Friday -> "\26143\26399\20116"
  Saturday -> "\26143\26399\20845"
  Sunday -> "\26143\26399\22825"

dayOfWeekJa :: DayOfWeek -> Text
dayOfWeekJa = \case
  Monday -> "\26376\26332\26085"
  Tuesday -> "\28779\26332\26085"
  Wednesday -> "\27700\26332\26085"
  Thursday -> "\26408\26332\26085"
  Friday -> "\37329\26332\26085"
  Saturday -> "\22303\26332\26085"
  Sunday -> "\26085\26332\26085"

intToDayOfWeek :: Int -> Maybe DayOfWeek
intToDayOfWeek = \case
  1 -> Just Monday
  2 -> Just Tuesday
  3 -> Just Wednesday
  4 -> Just Thursday
  5 -> Just Friday
  6 -> Just Saturday
  7 -> Just Sunday
  _ -> Nothing
