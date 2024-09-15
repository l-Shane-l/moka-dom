{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Tests.URL
Description : Tests for URL manipulation functions in Moka-DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module contains tests for the URL manipulation functions provided
by Moka-DOM. It tests getting the current URL, pushing new states,
and navigating through browser history.
-}
module Tests.URL( 
  testURLManipulation
) where

import Moka.DOM.URL
import Tests.Utils(logTestResult)
import qualified Data.Text as T

-- | Test suite for URL manipulation functions.
-- This function tests:
--   1. Getting the current URL
--   2. Pushing a new state
--   3. Navigating back in history
--   4. Navigating forward in history
testURLManipulation :: IO ()
testURLManipulation = do
  initialURL <- getCurrentURL
  logTestResult "URL Manipulation" $ "Initial URL: " <> initialURL
  
  -- Test pushing a new state
  pushState "/test-url"
  newURL <- getCurrentURL
  if "/test-url" `T.isSuffixOf` newURL
    then logTestResult "URL Manipulation" "pushState succeeded"
    else logTestResult "URL Manipulation" "pushState failed"
  
  -- Test going back in history
  goBack
  backURL <- getCurrentURL
  if backURL == initialURL
    then logTestResult "URL Manipulation" "goBack succeeded"
    else logTestResult "URL Manipulation" "goBack failed"
  
  -- Test going forward in history
  goForward
  forwardURL <- getCurrentURL
  if "/test-url" `T.isSuffixOf` forwardURL
    then logTestResult "URL Manipulation" "goForward succeeded"
    else logTestResult "URL Manipulation" "goForward failed"
