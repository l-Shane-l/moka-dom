{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Tests.Utils
Description : Utility functions for Moka-DOM tests
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module provides utility functions used across various test modules
in the Moka-DOM test suite. It includes functions for logging test results
and a placeholder for testing utility functions.
-}
module Tests.Utils(
   testUtils
 , logTestResult
) where

import Moka.DOM.Utils
import Moka.DOM.Elements
import qualified Data.Text as T

-- | Placeholder function for testing utility functions.
-- TODO: Implement actual utility function tests.
testUtils :: IO()
testUtils = consoleLog("Utils test");

-- | Log the result of a test to both the console and the DOM.
-- This function appends the test result to the body of the document
-- and logs it to the console for visibility during testing.
logTestResult :: T.Text -> T.Text -> IO ()
logTestResult testName result = do
  let logMessage = testName <> ": " <> result
  consoleLog logMessage
  appendToBody $ "<p>" <> logMessage <> "</p>"
