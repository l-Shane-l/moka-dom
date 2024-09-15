
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Tests.Cookies
Description : Tests for cookie manipulation functions in Moka-DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module contains tests for cookie manipulation functions provided by Moka-DOM,
including setting cookies, retrieving specific cookies, getting all cookies, and deleting cookies.
-}
module Tests.Cookies( 
  testCookies
) where

import Moka.DOM.Cookies
import Moka.DOM.Elements
import Tests.Utils(logTestResult)

-- | Test suite for cookie operations
-- This function tests:
-- - Setting a cookie
-- - Retrieving a specific cookie
-- - Getting all cookies
-- - Deleting a cookie
testCookies :: IO ()
testCookies = do
  let testKey = "testCookieKey"
      testValue = "testCookieValue"
  
  -- Set a cookie
  setCookie testKey testValue 1  -- Set for 1 day
  
  -- Get the cookie
  maybeCookie <- getCookie testKey
  case maybeCookie of
    Just retrievedValue ->
      if retrievedValue == testValue
        then do
          logTestResult "Cookie Test" "Passed (Set and Get successful)"
          appendToBody $ "<p>Stored value in cookie: " <> retrievedValue <> "</p>"
        else logTestResult "Cookie Test" "Failed (Retrieved value doesn't match)"
    Nothing -> logTestResult "Cookie Test" "Failed (Unable to retrieve cookie)"
  
  -- Get all cookies
  allCookies <- getAllCookies
  appendToBody $ "<p>All cookies: " <> allCookies <> "</p>"
  
  -- Delete the cookie
--  deleteCookie testKey
  
  -- Try to get the deleted cookie
  maybeDeletedCookie <- getCookie testKey
  case maybeDeletedCookie of
    Just _ -> logTestResult "Cookie Test" "Failed (Cookie not deleted)"
    Nothing -> logTestResult "Cookie Test" "Passed (Cookie successfully deleted)"
  
  logTestResult "Cookie Test" "Test complete. Check browser tools for cookie operations."
