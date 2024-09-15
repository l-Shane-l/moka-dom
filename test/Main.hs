{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : Test suite for Moka-DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module contains the main test suite for Moka-DOM. It runs a series of tests
covering various aspects of the library's functionality.
-}
module Main where

import Moka.DOM.Elements
import Moka.DOM.Utils
import Tests.Utils(testUtils)
import Tests.Elements(testAppendToBody, testCreateElement, testGetElementById)
import Tests.Events(testEventListener, testVisualChangeOnClick)
import Tests.Manipulation(testSetInnerHTML, testSetAttribute, testQuerySelector, testAddRemoveClass, testRemoveElement)
import Tests.Cookies(testCookies)
import Tests.Networking(testAjax)
import Tests.Storage(testLocalStorage)
import Tests.Helpers(testHelperFunctions)
import Tests.URL(testURLManipulation)

-- | The main entry point for the test suite.
main :: IO ()
main = do
  consoleLog "Starting Moka-DOM Core Tests"
  
  -- Setup the test environment
  setupTestEnvironment
  
  -- Run individual test suites
  testUtils            -- Test utility functions
  testElements         -- Test element creation and manipulation
  testManipulation     -- Test DOM manipulation functions
  testEvents           -- Test event handling
  testStorage          -- Test local storage operations
  testURLManipulation  -- Test URL manipulation functions
  testCookies          -- Test cookie operations
  testAjax             -- Test AJAX functionality
  testHelperFunctions  -- Test miscellaneous helper functions
  
  consoleLog "All Moka-DOM Core Tests completed"

-- | Set up the test environment by creating necessary DOM elements.
setupTestEnvironment :: IO ()
setupTestEnvironment = do
  appendToBody "<h1>Moka-DOM Core Tests</h1>"
  appendToBody "<div id='test-area'></div>"
  appendToBody "<div id='test-results'></div>"

-- | Run tests related to element creation and manipulation.
testElements :: IO ()
testElements = do
  consoleLog "Testing Element functions..."
  testAppendToBody
  testCreateElement
  testGetElementById

-- | Run tests related to DOM manipulation.
testManipulation :: IO ()
testManipulation = do
  consoleLog "Testing Manipulation functions..."
  testSetInnerHTML
  testSetAttribute
  testQuerySelector
  testAddRemoveClass
  testRemoveElement

-- | Run tests related to event handling.
testEvents :: IO ()
testEvents = do
  consoleLog "Testing Event functions..."
  testEventListener
  testVisualChangeOnClick

-- | Run tests related to local storage.
testStorage :: IO ()
testStorage = do
  consoleLog "Testing Storage functions..."
  testLocalStorage

