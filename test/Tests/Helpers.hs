{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Tests.Helpers
Description : Tests for helper functions in Moka-DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module contains tests for various helper functions provided by Moka-DOM,
including form operations, window dimension retrieval, document title manipulation,
and JSON parsing.
-}
module Tests.Helpers( 
  testHelperFunctions
) where

import Moka.DOM.Helpers
import Moka.DOM.Manipulation
import Moka.DOM.Elements
import Moka.DOM.Forms
import Tests.Utils(logTestResult)
import qualified Data.Text as T

-- | Test suite for helper functions
-- This function tests various helper functions including:
-- - Form operations (submit, reset, validity check, data retrieval)
-- - Window dimension retrieval
-- - Scrolling to elements
-- - Setting document title
-- - Getting current timestamp
-- - Parsing JSON
testHelperFunctions :: IO ()
testHelperFunctions = do
  logTestResult "Helper Functions Tests" "Starting Helper Functions tests"
  
  -- Test form functions
  maybeForm <- createElement "form"
  case maybeForm of
    Just form -> do
      -- Test submitForm (Note: This won't actually submit the form in a test environment)
      submitForm form
      logTestResult "submitForm" "Called (check browser console for any errors)"
      -- Test resetForm
      resetForm form
      logTestResult "resetForm" "Called (check browser console for any errors)"
      -- Test checkFormValidity
      isValid <- checkFormValidity form
      logTestResult "checkFormValidity" $ "Result: " <> T.pack (show isValid)
      -- Test getFormData (with an empty form)
      formData <- getFormData form
      logTestResult "getFormData" $ "Empty form data: " <> T.pack (show formData)
    Nothing -> logTestResult "Form Creation" "Failed to create form element"
  
  -- Test getWindowDimensions
  dimensions <- getWindowDimensions
  logTestResult "getWindowDimensions" $ "Result: " <> T.pack (show dimensions)
  
  -- Test scrollToElement (create a div and scroll to it)
  maybeDiv <- createElement "div"
  case maybeDiv of
    Just divEl -> do
      setInnerHTML divEl "Scroll Target"
      appendToBody "<div id='scroll-target'>Scroll Target</div>"
      maybeScrollTarget <- getElementById "scroll-target"
      case maybeScrollTarget of
        Just scrollTarget -> do
          scrollToElement scrollTarget
          logTestResult "scrollToElement" "Called (visually check if page scrolled)"
        Nothing -> logTestResult "Scroll Target" "Failed to get scroll target element"
    Nothing -> logTestResult "Div Creation" "Failed to create div element"
  
  -- Test setDocumentTitle
  setDocumentTitle "Moka-DOM Test Title"
  logTestResult "setDocumentTitle" "Set to 'Moka-DOM Test Title' (check browser tab)"
  
  -- Test getCurrentTimestamp
  timestamp <- getCurrentTimestamp
  logTestResult "getCurrentTimestamp" $ "Result: " <> T.pack (show timestamp)
  
  -- Test parseJSON
  let validJSON = "{\"key\": \"value\"}"
      invalidJSON = "{\"key\": \"value\""
  parsedValid <- parseJSON validJSON
  parsedInvalid <- parseJSON invalidJSON
  logTestResult "parseJSON (valid)" $ "Result: " <> T.pack (show parsedValid)
  logTestResult "parseJSON (invalid)" $ "Result: " <> T.pack (show parsedInvalid)
  
  logTestResult "Helper Functions Tests" "All Helper Functions tests completed"
