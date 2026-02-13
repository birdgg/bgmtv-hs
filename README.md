# bgmtv-hs

A type-safe Haskell client library for the [BGM.tv](https://bgm.tv) API using servant-client.

## Installation

Add to your cabal file:

```cabal
build-depends: bgmtv
```

## Quick Start

```haskell
import Web.Bgmtv

main :: IO ()
main = do
  client <- newBgmtvClient (defaultConfig "my-app/1.0")

  -- Search subjects
  result <- client.searchSubjects (SearchRequest "葬送的芙莉蓮" Nothing Nothing Nothing)
  case result of
    Right resp -> mapM_ print resp.list
    Left err -> print err

  -- Get subject details (type-safe ID)
  detail <- client.getSubject (SubjectId 425251)
  print detail

  -- Get episodes with pagination
  episodes <- client.getEpisodes (SubjectId 425251) (Just 100) (Just 0)
  print episodes

  -- Get daily airing schedule
  calendar <- client.getCalendar
  print calendar
```

## Client Creation

```haskell
-- Simple: auto-managed HTTP connection
client <- newBgmtvClient (defaultConfig "my-app/1.0")

-- Advanced: custom HTTP Manager (shared connection pool, custom TLS, etc.)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

manager <- newManager tlsManagerSettings
let client = newBgmtvClientWith manager (defaultConfig "my-app/1.0")
```

## BgmtvClient API

| Method | Type | Description |
|--------|------|-------------|
| `searchSubjects` | `SearchRequest -> IO (Response SearchResponse)` | Search subjects with filters |
| `getSubject` | `SubjectId -> IO (Response SubjectDetail)` | Get subject details by ID |
| `getEpisodes` | `SubjectId -> Maybe Int64 -> Maybe Int64 -> IO (Response EpisodesResponse)` | Get episodes with pagination |
| `getCalendar` | `IO (Response [CalendarItem])` | Get daily airing schedule |

## Modules

| Module | Description |
|--------|-------------|
| `Web.Bgmtv` | Main entry point, re-exports all public API |
| `Web.Bgmtv.Types.Id` | Type-safe ID wrappers (`SubjectId`, `EpisodeId`) |
| `Web.Bgmtv.Types.Enums` | Enum types (`SubjectType`, `EpisodeType`, `Platform`) |
| `Web.Bgmtv.Types.Subject` | Subject types (`Subject`, `SubjectDetail`, `LegacySubject`, `Rating`) |
| `Web.Bgmtv.Types.Episode` | Episode types (`Episode`, `EpisodesResponse`) |
| `Web.Bgmtv.Types.Search` | Search types (`SearchRequest`, `SearchFilter`, `SearchResponse`) |
| `Web.Bgmtv.Types.Error` | Error types (`BgmtvError`, `ApiError`, `ErrorDetails`, `Response`) |
| `Web.Bgmtv.Types.Calendar` | Calendar types (`CalendarItem`) |
| `Web.Bgmtv.API` | Servant API type definition |
| `Web.Bgmtv.Client` | Client implementation |

## Error Handling

All client methods return `Response a` (alias for `Either BgmtvError a`). Errors use a two-layer pattern separating network errors from domain errors:

```haskell
data BgmtvError
  = ServantError ClientError     -- Network/HTTP layer (raw servant-client error)
  | BgmtvApiError ApiError       -- Structured API error (parsed from JSON response)
```

`ApiError` contains the structured error body returned by the BGM.tv API:

```haskell
data ApiError = ApiError
  { title       :: Text          -- Error title (e.g. "Not Found")
  , details     :: ErrorDetails  -- Request path and method
  , requestId   :: Text          -- Unique request ID for debugging
  , description :: Text          -- Human-readable error description
  }
```

```haskell
result <- client.getSubject (SubjectId 999999)
case result of
  Right subject -> putStrLn subject.name
  Left (BgmtvApiError apiErr) ->
    putStrLn $ "API error: " <> apiErr.title <> " - " <> apiErr.description
  Left (ServantError err) ->
    putStrLn $ "Network error: " <> show err
```

## Type-safe IDs

ID types use newtype wrappers to prevent mixing up different identifiers at compile time:

```haskell
let sid = SubjectId 12345   -- Subject ID
let eid = EpisodeId 67890   -- Episode ID

-- These are different types; the compiler prevents misuse
getSubject client sid   -- OK
-- getSubject client eid  -- Compile error!
```

## License

MIT
