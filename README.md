# bgmtv-hs

A type-safe Haskell client library for the [BGM.tv](https://bgm.tv) API using servant-client.

## Installation

Add to your cabal file:

```cabal
build-depends: bgmtv
```

## Usage

```haskell
import Web.Bgmtv

main :: IO ()
main = do
  let config = defaultConfig "your-app/1.0"

  -- Search for anime
  result <- searchAnime config "葬送的芙莉蓮"
  case result of
    Right subjects -> mapM_ print subjects
    Left err -> print err

  -- Get subject details
  detail <- runBgmtv config (getSubjectM 425249)
  print detail

  -- Get all episodes with auto-pagination
  episodes <- getAllEpisodes config 425249
  print episodes
```

## API Coverage

| Endpoint | Function | Description |
|----------|----------|-------------|
| `POST /v0/search/subjects` | `searchSubjectsM` | Search subjects |
| `GET /v0/subjects/{id}` | `getSubjectM` | Get subject details |
| `GET /v0/episodes` | `getEpisodesM` | Get episodes |

## High-level Functions

- `searchAnime` - Search for Japanese anime
- `getAllEpisodes` - Get all episodes with automatic pagination

## License

MIT
