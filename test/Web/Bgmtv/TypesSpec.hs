module Web.Bgmtv.TypesSpec (spec) where

import Data.Aeson
import Data.Text (Text)
import Data.Time (DayOfWeek (..), fromGregorian)
import Test.Hspec
import Web.Bgmtv.Types.Calendar
import Web.Bgmtv.Types.Enums
import Web.Bgmtv.Types.Episode
import Web.Bgmtv.Types.Id
import Web.Bgmtv.Types.Search
import Web.Bgmtv.Types.Subject

spec :: Spec
spec = do
  idTypesSpec
  subjectTypeSpec
  episodeTypeSpec
  searchRequestSpec
  responseParsingSpec
  calendarParsingSpec

-- | ID newtype JSON encoding tests
idTypesSpec :: Spec
idTypesSpec = describe "ID Types" $ do
  describe "SubjectId" $ do
    it "encodes to JSON number" $
      toJSON (SubjectId 12345) `shouldBe` Number 12345

    it "decodes from JSON number" $
      fromJSON (Number 12345) `shouldBe` Success (SubjectId 12345)

    it "roundtrips through JSON" $ do
      let sid = SubjectId 99999
      fromJSON (toJSON sid) `shouldBe` Success sid

  describe "EpisodeId" $ do
    it "encodes to JSON number" $
      toJSON (EpisodeId 67890) `shouldBe` Number 67890

    it "decodes from JSON number" $
      fromJSON (Number 67890) `shouldBe` Success (EpisodeId 67890)

    it "roundtrips through JSON" $ do
      let eid = EpisodeId 11111
      fromJSON (toJSON eid) `shouldBe` Success eid

-- | SubjectType JSON encoding tests
subjectTypeSpec :: Spec
subjectTypeSpec = describe "SubjectType" $ do
  describe "toJSON" $ do
    it "encodes Book as 1" $
      toJSON Book `shouldBe` Number 1

    it "encodes Anime as 2" $
      toJSON Anime `shouldBe` Number 2

    it "encodes Music as 3" $
      toJSON Music `shouldBe` Number 3

    it "encodes Game as 4" $
      toJSON Game `shouldBe` Number 4

    it "encodes Real as 6" $
      toJSON Real `shouldBe` Number 6

  describe "fromJSON" $ do
    it "decodes 1 as Book" $
      fromJSON (Number 1) `shouldBe` Success Book

    it "decodes 2 as Anime" $
      fromJSON (Number 2) `shouldBe` Success Anime

    it "decodes 3 as Music" $
      fromJSON (Number 3) `shouldBe` Success Music

    it "decodes 4 as Game" $
      fromJSON (Number 4) `shouldBe` Success Game

    it "decodes 6 as Real" $
      fromJSON (Number 6) `shouldBe` Success Real

    it "fails on unknown type" $
      case fromJSON @SubjectType (Number 99) of
        Error _ -> pure ()
        Success _ -> expectationFailure "should fail on unknown type"

-- | EpisodeType JSON encoding tests
episodeTypeSpec :: Spec
episodeTypeSpec = describe "EpisodeType" $ do
  describe "toJSON" $ do
    it "encodes Main as 0" $
      toJSON Main `shouldBe` Number 0

    it "encodes Special as 1" $
      toJSON Special `shouldBe` Number 1

    it "encodes Opening as 2" $
      toJSON Opening `shouldBe` Number 2

    it "encodes Ending as 3" $
      toJSON Ending `shouldBe` Number 3

  describe "fromJSON" $ do
    it "decodes 0 as Main" $
      fromJSON (Number 0) `shouldBe` Success Main

    it "decodes 1 as Special" $
      fromJSON (Number 1) `shouldBe` Success Special

    it "decodes 2 as Opening" $
      fromJSON (Number 2) `shouldBe` Success Opening

    it "decodes 3 as Ending" $
      fromJSON (Number 3) `shouldBe` Success Ending

-- | SearchRequest serialization tests
searchRequestSpec :: Spec
searchRequestSpec = describe "SearchRequest" $ do
  it "serializes keyword only request" $ do
    let req = SearchRequest "test" Nothing
    toJSON req `shouldBe` object ["keyword" .= ("test" :: Text)]

  it "serializes request with filter" $ do
    let filter' = SearchFilter (Just [Anime]) Nothing Nothing
        req = SearchRequest "test" (Just filter')
        expected = object
          [ "keyword" .= ("test" :: Text)
          , "filter" .= object ["type" .= [Number 2]]
          ]
    toJSON req `shouldBe` expected

-- | Response type parsing tests
responseParsingSpec :: Spec
responseParsingSpec = describe "Response parsing" $ do
  describe "Subject" $ do
    it "parses valid subject JSON" $ do
      let json = object
            [ "id" .= (12345 :: Int)
            , "name" .= ("Test Name" :: Text)
            , "name_cn" .= ("测试名称" :: Text)
            , "date" .= ("2024-01-01" :: Text)
            , "platform" .= ("TV" :: Text)
            , "images" .= object
                [ "small" .= ("http://example.com/s.jpg" :: Text)
                , "grid" .= ("http://example.com/g.jpg" :: Text)
                , "large" .= ("http://example.com/l.jpg" :: Text)
                , "medium" .= ("http://example.com/m.jpg" :: Text)
                , "common" .= ("http://example.com/c.jpg" :: Text)
                ]
            , "image" .= ("http://example.com/default.jpg" :: Text)
            , "eps" .= (12 :: Int)
            ]
      case fromJSON json of
        Success (s :: Subject) -> do
          s.id `shouldBe` SubjectId 12345
          s.name `shouldBe` "Test Name"
          s.nameCn `shouldBe` "测试名称"
          s.date `shouldBe` Just (fromGregorian 2024 1 1)
          s.eps `shouldBe` 12
        Error e -> expectationFailure $ "Failed to parse Subject: " <> e

    it "parses subject with empty date as Nothing" $ do
      let json = object
            [ "id" .= (1 :: Int)
            , "name" .= ("Test" :: Text)
            , "name_cn" .= ("" :: Text)
            , "date" .= ("" :: Text)
            , "platform" .= ("TV" :: Text)
            , "images" .= object
                [ "small" .= ("" :: Text), "grid" .= ("" :: Text)
                , "large" .= ("" :: Text), "medium" .= ("" :: Text)
                , "common" .= ("" :: Text)
                ]
            , "eps" .= (0 :: Int)
            ]
      case fromJSON json of
        Success (s :: Subject) -> s.date `shouldBe` Nothing
        Error e -> expectationFailure $ "Failed to parse Subject with empty date: " <> e

    it "parses subject with 0000-00-00 date as Nothing" $ do
      let json = object
            [ "id" .= (1 :: Int)
            , "name" .= ("Test" :: Text)
            , "name_cn" .= ("" :: Text)
            , "date" .= ("0000-00-00" :: Text)
            , "platform" .= ("TV" :: Text)
            , "images" .= object
                [ "small" .= ("" :: Text), "grid" .= ("" :: Text)
                , "large" .= ("" :: Text), "medium" .= ("" :: Text)
                , "common" .= ("" :: Text)
                ]
            , "eps" .= (0 :: Int)
            ]
      case fromJSON json of
        Success (s :: Subject) -> s.date `shouldBe` Nothing
        Error e -> expectationFailure $ "Failed to parse Subject with 0000-00-00 date: " <> e

  describe "Episode" $ do
    it "parses valid episode JSON" $ do
      let json = object
            [ "id" .= (1001 :: Int)
            , "type" .= (0 :: Int)
            , "name" .= ("Episode 1" :: Text)
            , "name_cn" .= ("第1集" :: Text)
            , "sort" .= (1.0 :: Double)
            , "ep" .= (1.0 :: Double)
            , "airdate" .= ("2024-01-08" :: Text)
            ]
      case fromJSON json of
        Success (ep :: Episode) -> do
          ep.id `shouldBe` EpisodeId 1001
          ep.episodeType `shouldBe` Main
          ep.name `shouldBe` "Episode 1"
          ep.airdate `shouldBe` Just (fromGregorian 2024 1 8)
          ep.sort `shouldBe` 1.0
        Error e -> expectationFailure $ "Failed to parse Episode: " <> e

    it "parses episode with missing airdate" $ do
      let json = object
            [ "id" .= (1002 :: Int)
            , "type" .= (0 :: Int)
            , "name" .= ("Episode 2" :: Text)
            , "name_cn" .= ("第2集" :: Text)
            , "sort" .= (2.0 :: Double)
            ]
      case fromJSON json of
        Success (ep :: Episode) ->
          ep.airdate `shouldBe` Nothing
        Error e -> expectationFailure $ "Failed to parse Episode without airdate: " <> e

  describe "SearchResponse" $ do
    it "parses valid search response" $ do
      let subjectJson = object
            [ "id" .= (1 :: Int)
            , "name" .= ("Name" :: Text)
            , "name_cn" .= ("名称" :: Text)
            , "date" .= ("2024-01-01" :: Text)
            , "platform" .= ("TV" :: Text)
            , "images" .= object
                [ "small" .= ("" :: Text)
                , "grid" .= ("" :: Text)
                , "large" .= ("" :: Text)
                , "medium" .= ("" :: Text)
                , "common" .= ("" :: Text)
                ]
            , "image" .= ("" :: Text)
            , "eps" .= (1 :: Int)
            ]
          json = object
            [ "total" .= (100 :: Int)
            , "limit" .= (10 :: Int)
            , "offset" .= (0 :: Int)
            , "data" .= [subjectJson]
            ]
      case fromJSON json of
        Success (r :: SearchResponse) -> do
          r.total `shouldBe` 100
          r.limit `shouldBe` 10
          r.offset `shouldBe` 0
          length r.data_ `shouldBe` 1
        Error e -> expectationFailure $ "Failed to parse SearchResponse: " <> e

  describe "EpisodesResponse" $ do
    it "parses valid episodes response" $ do
      let episodeJson = object
            [ "id" .= (1 :: Int)
            , "type" .= (0 :: Int)
            , "name" .= ("Ep" :: Text)
            , "name_cn" .= ("集" :: Text)
            , "sort" .= (1.0 :: Double)
            , "airdate" .= ("2024-01-01" :: Text)
            ]
          json = object
            [ "data" .= [episodeJson]
            , "total" .= (24 :: Int)
            , "limit" .= (100 :: Int)
            , "offset" .= (0 :: Int)
            ]
      case fromJSON json of
        Success (r :: EpisodesResponse) -> do
          r.total `shouldBe` 24
          length r.data_ `shouldBe` 1
        Error e -> expectationFailure $ "Failed to parse EpisodesResponse: " <> e

-- | Calendar type parsing tests
calendarParsingSpec :: Spec
calendarParsingSpec = describe "Calendar parsing" $ do
  describe "CalendarItem weekday parsing" $ do
    it "parses weekday id 1 as Monday" $ do
      let json = object
            [ "weekday" .= object
                [ "id" .= (1 :: Int)
                , "en" .= ("Mon" :: Text)
                , "cn" .= ("\26143\26399\19968" :: Text)
                , "ja" .= ("\26376\32768\26085" :: Text)
                ]
            , "items" .= ([] :: [Value])
            ]
      case fromJSON json of
        Success (ci :: CalendarItem) -> do
          ci.weekday `shouldBe` Monday
          ci.items `shouldBe` []
        Error e -> expectationFailure $ "Failed to parse CalendarItem: " <> e

  describe "RatingCount" $ do
    it "parses numeric string keys" $ do
      let json = object
            [ "1" .= (5 :: Int), "2" .= (3 :: Int), "3" .= (4 :: Int)
            , "4" .= (6 :: Int), "5" .= (46 :: Int), "6" .= (267 :: Int)
            , "7" .= (659 :: Int), "8" .= (885 :: Int), "9" .= (284 :: Int)
            , "10" .= (130 :: Int)
            ]
      case fromJSON json of
        Success (rc :: RatingCount) -> do
          rc.r1 `shouldBe` 5
          rc.r10 `shouldBe` 130
        Error e -> expectationFailure $ "Failed to parse RatingCount: " <> e

    it "roundtrips through JSON" $ do
      let rc = RatingCount 5 3 4 6 46 267 659 885 284 130
      fromJSON (toJSON rc) `shouldBe` Success rc

  describe "SubjectCollection" $ do
    it "parses with snake_case keys" $ do
      let json = object
            [ "wish" .= (608 :: Int)
            , "collect" .= (3010 :: Int)
            , "doing" .= (103 :: Int)
            , "on_hold" .= (284 :: Int)
            , "dropped" .= (86 :: Int)
            ]
      case fromJSON json of
        Success (sc :: SubjectCollection) -> do
          sc.wish `shouldBe` 608
          sc.onHold `shouldBe` 284
        Error e -> expectationFailure $ "Failed to parse SubjectCollection: " <> e

    it "roundtrips through JSON" $ do
      let sc = SubjectCollection 608 3010 103 284 86
      fromJSON (toJSON sc) `shouldBe` Success sc

  describe "LegacySubject" $ do
    it "parses with all fields present" $ do
      let json = object
            [ "id" .= (12 :: Int)
            , "url" .= ("https://bgm.tv/subject/12" :: Text)
            , "type" .= (2 :: Int)
            , "name" .= ("\12385\12423\12403\12387\12484" :: Text)
            , "name_cn" .= ("\20154\24418\30005\33041\22825\20351\24515" :: Text)
            , "summary" .= ("summary text" :: Text)
            , "air_date" .= ("2002-04-02" :: Text)
            , "air_weekday" .= (2 :: Int)
            , "images" .= object
                [ "large" .= ("https://example.com/l.jpg" :: Text)
                , "common" .= ("https://example.com/c.jpg" :: Text)
                , "medium" .= ("https://example.com/m.jpg" :: Text)
                , "small" .= ("https://example.com/s.jpg" :: Text)
                , "grid" .= ("https://example.com/g.jpg" :: Text)
                ]
            , "eps" .= (27 :: Int)
            , "eps_count" .= (27 :: Int)
            , "rating" .= object
                [ "total" .= (2289 :: Int)
                , "count" .= object
                    [ "1" .= (5 :: Int), "2" .= (3 :: Int), "3" .= (4 :: Int)
                    , "4" .= (6 :: Int), "5" .= (46 :: Int), "6" .= (267 :: Int)
                    , "7" .= (659 :: Int), "8" .= (885 :: Int), "9" .= (284 :: Int)
                    , "10" .= (130 :: Int)
                    ]
                , "score" .= (7.6 :: Double)
                ]
            , "rank" .= (573 :: Int)
            , "collection" .= object
                [ "wish" .= (608 :: Int)
                , "collect" .= (3010 :: Int)
                , "doing" .= (103 :: Int)
                , "on_hold" .= (284 :: Int)
                , "dropped" .= (86 :: Int)
                ]
            ]
      case fromJSON json of
        Success (s :: LegacySubject) -> do
          s.id `shouldBe` SubjectId 12
          s.subjectType `shouldBe` Anime
          s.airDate `shouldBe` Just (fromGregorian 2002 4 2)
          s.rank `shouldBe` Just 573
        Error e -> expectationFailure $ "Failed to parse LegacySubject: " <> e

    it "parses with optional fields missing" $ do
      let json = object
            [ "id" .= (12 :: Int)
            , "url" .= ("https://bgm.tv/subject/12" :: Text)
            , "type" .= (2 :: Int)
            , "name" .= ("Test" :: Text)
            , "name_cn" .= ("" :: Text)
            , "summary" .= ("" :: Text)
            ]
      case fromJSON json of
        Success (s :: LegacySubject) -> do
          s.id `shouldBe` SubjectId 12
          s.images `shouldBe` Nothing
          s.rating `shouldBe` Nothing
          s.rank `shouldBe` Nothing
          s.collection `shouldBe` Nothing
        Error e -> expectationFailure $ "Failed to parse LegacySubject: " <> e

  describe "CalendarItem" $ do
    it "parses valid calendar item" $ do
      let subjectJson = object
            [ "id" .= (12 :: Int)
            , "url" .= ("https://bgm.tv/subject/12" :: Text)
            , "type" .= (2 :: Int)
            , "name" .= ("Test" :: Text)
            , "name_cn" .= ("" :: Text)
            , "summary" .= ("" :: Text)
            ]
          json = object
            [ "weekday" .= object
                [ "id" .= (1 :: Int)
                , "en" .= ("Mon" :: Text)
                , "cn" .= ("\26143\26399\19968" :: Text)
                , "ja" .= ("\26376\32768\26085" :: Text)
                ]
            , "items" .= [subjectJson]
            ]
      case fromJSON json of
        Success (ci :: CalendarItem) -> do
          ci.weekday `shouldBe` Monday
          length ci.items `shouldBe` 1
        Error e -> expectationFailure $ "Failed to parse CalendarItem: " <> e
