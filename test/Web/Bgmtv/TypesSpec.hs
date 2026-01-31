module Web.Bgmtv.TypesSpec (spec) where

import Data.Aeson
import Data.Text (Text)
import Test.Hspec
import Web.Bgmtv.Types

spec :: Spec
spec = do
  idTypesSpec
  subjectTypeSpec
  episodeTypeSpec
  searchRequestSpec
  responseParsingSpec

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
          s.eps `shouldBe` 12
        Error e -> expectationFailure $ "Failed to parse Subject: " <> e

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
          ep.sort `shouldBe` 1.0
        Error e -> expectationFailure $ "Failed to parse Episode: " <> e

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
