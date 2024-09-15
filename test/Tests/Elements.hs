
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Tests.Elements
Description : Tests for element manipulation functions in Moka-DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module contains tests for element manipulation functions provided by Moka-DOM,
including getting elements by ID, creating new elements, and appending elements to the body.
-}
module Tests.Elements( 
   testGetElementById
 , testCreateElement
 , testAppendToBody
) where

import Moka.DOM.Elements
import Moka.DOM.Manipulation
import Tests.Utils(logTestResult)

-- | Test appending an element to the document body
testAppendToBody :: IO ()
testAppendToBody = do
  appendToBody "<p id='append-test'>Test Append To Body</p>"
  maybeEl <- getElementById "append-test"
  case maybeEl of
    Just _ -> logTestResult "Append to Body" "Passed"
    Nothing -> logTestResult "Append to Body" "Failed"

-- | Test creating a new element and adding it to the DOM
testCreateElement :: IO ()
testCreateElement = do
  maybeEl <- createElement "div"
  case maybeEl of
    Just el -> do
      setAttribute el "id" "created-div"
      setInnerHTML el "Created Element Test"
      maybeTestArea <- getElementById "test-area"
      case maybeTestArea of
        Just testArea -> do
          appendElement testArea el
          logTestResult "Create Element" "Passed"
        Nothing -> logTestResult "Create Element" "Failed (Test area not found)"
    Nothing -> logTestResult "Create Element" "Failed"

-- | Test retrieving an element by its ID
testGetElementById :: IO ()
testGetElementById = do
  appendToBody "<div id='test-get-element'>Get Element By ID Test</div>"
  maybeEl <- getElementById "test-get-element"
  case maybeEl of
    Just _ -> logTestResult "Get Element By ID" "Passed"
    Nothing -> logTestResult "Get Element By ID" "Failed"
