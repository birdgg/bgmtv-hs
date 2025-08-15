{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module APITest (tests) where

import BgmTV
import Network.HTTP.Client.TLS
import Servant.Client
import Test.Tasty
import Test.Tasty.HUnit

showClientError :: ClientError -> String
showClientError (FailureResponse _ _) = "FailureResponse"
showClientError (DecodeFailure t _) = "DecodeFailure: " <> show t
showClientError (UnsupportedContentType _ _) = "UnsupportedContentType"
showClientError (InvalidContentTypeHeader _) = "InvalidContentTypeHeader"
showClientError (ConnectionError _) = "ConnectionError"

tests :: TestTree
tests =
  testGroup
    "API Integration Tests"
    [ testCase "Calendar API should return valid data" testCalendarApi
    ]

testCalendarApi :: Assertion
testCalendarApi = do
  manager <- newTlsManager
  let clientEnv = mkBgmClientEnv "bangumi-hs" manager
  result <- runClientM getCalendar clientEnv
  case result of
    Left err -> assertFailure $ "API call failed: " ++ showClientError err
    Right calendars -> do
      assertBool "Should return non-negative number of calendars" (length calendars == 7)
