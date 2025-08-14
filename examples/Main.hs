module Main where

import BgmTV.Types.Subject
import Data.Aeson

-- Example Subject data for testing
exampleSubject :: Subject
exampleSubject = Subject
  { id = 12345
  , url = "https://bgm.tv/subject/12345"
  , stype = [Anime]
  , name = "Example Anime"
  , nameCn = "示例动画"
  , summary = "This is an example anime for testing"
  , airDate = "2024-01-01"
  , airWeekday = 1
  , images = SubjectImages
      { large = "https://lain.bgm.tv/pic/cover/l/12345.jpg"
      , common = "https://lain.bgm.tv/pic/cover/c/12345.jpg"
      , medium = "https://lain.bgm.tv/pic/cover/m/12345.jpg"
      , small = "https://lain.bgm.tv/pic/cover/s/12345.jpg"
      , grid = "https://lain.bgm.tv/pic/cover/g/12345.jpg"
      }
  , eps = 12
  , epsCount = 12
  , rating = SubjectRating
      { total = 1000
      , score = 7.5
      }
  , rank = 100
  }

main :: IO ()
main = do
  putStrLn "BgmTV Examples"
  putStrLn "=============="
  putStrLn ""
  
  -- Test JSON encoding
  putStrLn "Subject JSON encoding:"
  putStrLn $ show $ encode exampleSubject
  putStrLn ""
  
  -- Test SubjectType encoding/decoding
  putStrLn "SubjectType encoding:"
  putStrLn $ "Book: " ++ show (encode Book)
  putStrLn $ "Anime: " ++ show (encode Anime)
  putStrLn $ "Music: " ++ show (encode Music)
  putStrLn $ "Game: " ++ show (encode Game)
  putStrLn $ "Other: " ++ show (encode Other)
  putStrLn ""
  
  -- Test JSON decoding
  putStrLn "SubjectType decoding:"
  let animeJson = encode Anime
  case decode animeJson of
    Just subjectType -> putStrLn $ "Decoded: " ++ show (subjectType :: SubjectType)
    Nothing -> putStrLn "Failed to decode"