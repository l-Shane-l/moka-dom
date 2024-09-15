{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Tests.Networking
Description : Tests for AJAX functions in Moka-DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module contains tests for the AJAX functions provided by Moka-DOM.
It tests GET, POST, PUT, and DELETE requests using httpbin.org as a test endpoint.
-}
module Tests.Networking( 
  testAjax
) where

import Moka.DOM.Networking
import Moka.DOM.Elements
import Tests.Utils(logTestResult)
import Data.Text (Text)
import Data.Aeson (object, (.=))

-- | Test suite for AJAX functions.
-- This function tests:
--   1. GET request
--   2. POST request with JSON payload
--   3. PUT request with JSON payload
--   4. DELETE request
-- All tests use httpbin.org as the test endpoint.
testAjax :: IO ()
testAjax = do
  logTestResult "AJAX Tests" "Starting AJAX tests"
  
  -- Test GET request
  ajaxGet "https://httpbin.org/get" $ \maybeResult ->
    case maybeResult of
      Just result -> do
        logTestResult "AJAX GET" "Passed"
        appendToBody $ "<p>GET result: " <> result <> "</p>"
      Nothing -> logTestResult "AJAX GET" "Failed"
  
  -- Test POST request
  let postData = object [ "title" .= ("foo" :: Text)
                        , "body" .= ("bar" :: Text)
                        , "userId" .= (1 :: Int)
                        ]
  ajaxPost "https://httpbin.org/post" postData $ \maybeResult ->
    case maybeResult of
      Just result -> do
        logTestResult "AJAX POST" "Passed"
        appendToBody $ "<p>POST result: " <> result <> "</p>"
      Nothing -> logTestResult "AJAX POST" "Failed"
  
  -- Test PUT request
  let putData = object [ "id" .= (1 :: Int)
                       , "title" .= ("foo updated" :: Text)
                       , "body" .= ("bar updated" :: Text)
                       , "userId" .= (1 :: Int)
                       ]
  ajaxPut "https://httpbin.org/put" putData $ \maybeResult ->
    case maybeResult of
      Just result -> do
        logTestResult "AJAX PUT" "Passed"
        appendToBody $ "<p>PUT result: " <> result <> "</p>"
      Nothing -> logTestResult "AJAX PUT" "Failed"
  
  -- Test DELETE request
  ajaxDelete "https://httpbin.org/delete" $ \maybeResult ->
    case maybeResult of
      Just result -> do
        logTestResult "AJAX DELETE" "Passed"
        appendToBody $ "<p>DELETE result: " <> result <> "</p>"
      Nothing -> logTestResult "AJAX DELETE" "Failed"
  
  logTestResult "AJAX Tests" "All AJAX tests completed"
