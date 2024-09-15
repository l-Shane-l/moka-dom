{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Tests.Storage
Description : Tests for local storage functions in Moka-DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module contains tests for the local storage functions provided
by Moka-DOM. It tests setting and retrieving items from local storage.
-}
module Tests.Storage( 
  testLocalStorage
) where

import Moka.DOM.Storage
import Moka.DOM.Elements
import Tests.Utils(logTestResult)

-- | Test suite for local storage functions.
-- This function tests:
--   1. Setting an item in local storage
--   2. Retrieving an item from local storage
--   3. Verifying the retrieved value matches the set value
testLocalStorage :: IO ()
testLocalStorage = do
  let testKey = "testStorageKey"
      testValue = "testStorageValue"
  
  setLocalStorageItem testKey testValue
  maybeRetrievedValue <- getLocalStorageItem testKey
  case maybeRetrievedValue of
    Just retrievedValue ->
      if retrievedValue == testValue
        then do
          logTestResult "Local Storage" "Passed (Set and Get successful)"
          appendToBody $ "<p>Stored value in local storage: " <> retrievedValue <> "</p>"
        else logTestResult "Local Storage" "Failed (Retrieved value doesn't match)"
    Nothing -> logTestResult "Local Storage" "Failed (Unable to retrieve value)"
  
  logTestResult "Local Storage" "Test complete. Check browser tools for 'testStorageKey' in Local Storage."
