{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Tests.Manipulation
Description : Tests for DOM manipulation functions in Moka-DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module contains tests for various DOM manipulation functions provided by Moka-DOM,
including setting inner HTML, manipulating attributes, querying elements, and
adding/removing CSS classes.
-}
module Tests.Manipulation( 
    testSetInnerHTML
  , testSetAttribute
  , testQuerySelector
  , testAddRemoveClass
  , testRemoveElement
) where

import Moka.DOM.Manipulation
import Moka.DOM.Elements
import Tests.Utils(logTestResult)
import qualified Data.Text as T

-- | Test setting and getting inner HTML of an element
testSetInnerHTML :: IO ()
testSetInnerHTML = do
  maybeEl <- createElement "p"
  case maybeEl of
    Just el -> do
      setInnerHTML el "Test Set Inner HTML"
      content <- getInnerHTML el
      if content == "Test Set Inner HTML"
        then logTestResult "Set Inner HTML" "Passed"
        else logTestResult "Set Inner HTML" "Failed"
    Nothing -> logTestResult "Set Inner HTML" "Failed (Element creation failed)"

-- | Test setting and getting attributes of an element
testSetAttribute :: IO ()
testSetAttribute = do
  maybeEl <- createElement "button"
  case maybeEl of
    Just el -> do
      setAttribute el "class" "test-button"
      maybeClass <- getAttribute el "class"
      case maybeClass of
        Just classValue ->
          if classValue == "test-button"
            then logTestResult "Set Attribute" "Passed"
            else logTestResult "Set Attribute" "Failed (Unexpected attribute value)"
        Nothing -> logTestResult "Set Attribute" "Failed (Attribute not set)"
    Nothing -> logTestResult "Set Attribute" "Failed (Element creation failed)"

-- | Test querying elements using CSS selectors
testQuerySelector :: IO ()
testQuerySelector = do
  appendToBody "<div class='test-query-selector'>Query Selector Test</div>"
  maybeEl <- querySelector ".test-query-selector"
  case maybeEl of
    Just _ -> logTestResult "Query Selector" "Passed"
    Nothing -> logTestResult "Query Selector" "Failed"

-- | Test adding and removing CSS classes from an element
testAddRemoveClass :: IO ()
testAddRemoveClass = do
  maybeEl <- createElement "div"
  case maybeEl of
    Just el -> do
      addClass el "test-class"
      maybeClass <- getAttribute el "class"
      case maybeClass of
        Just classValue ->
          if "test-class" `T.isInfixOf` classValue
            then do
              removeClass el "test-class"
              maybeClassAfterRemove <- getAttribute el "class"
              case maybeClassAfterRemove of
                Just classValueAfterRemove ->
                  if "test-class" `T.isInfixOf` classValueAfterRemove
                    then logTestResult "Add/Remove Class" "Failed (Class not removed)"
                    else logTestResult "Add/Remove Class" "Passed"
                Nothing -> logTestResult "Add/Remove Class" "Passed"
            else logTestResult "Add/Remove Class" "Failed (Class not added)"
        Nothing -> logTestResult "Add/Remove Class" "Failed (Class not added)"
    Nothing -> logTestResult "Add/Remove Class" "Failed (Element creation failed)"

-- | Test removing an element from the DOM
testRemoveElement :: IO ()
testRemoveElement = do
  maybeParent <- createElement "div"
  maybeChild <- createElement "p"
  case (maybeParent, maybeChild) of
    (Just parent, Just child) -> do
      appendElement parent child
      removeElement child
      content <- getInnerHTML parent
      if T.null content
        then logTestResult "Remove Element" "Passed"
        else logTestResult "Remove Element" "Failed (Child not removed)"
    _ -> logTestResult "Remove Element" "Failed (Element creation failed)"
