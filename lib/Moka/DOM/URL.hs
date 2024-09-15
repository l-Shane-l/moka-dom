{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Moka.DOM.URL
Description : URL manipulation functions for Moka DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module provides functions for manipulating and interacting with the browser's URL
and history. It includes functionality for getting the current URL, pushing new states,
and navigating through the browser history.
-}
module Moka.DOM.URL
  ( getCurrentURL
  , pushState
  , goBack
  , goForward
  ) where

import Moka.DOM.FFI.URL_FFI(js_pushState, js_goForward, js_goBack, js_getCurrentURL)
import GHC.JS.Prim
import Data.Text (Text)
import qualified Data.Text as T

-- | Get the current URL of the page.
getCurrentURL :: IO Text
getCurrentURL = do
  url <- js_getCurrentURL
  return $ T.pack $ fromJSString url

-- | Push a new URL to the browser history without reloading the page.
--   This updates the URL displayed in the browser's address bar.
pushState :: Text -> IO ()
pushState url = js_pushState (toJSString $ T.unpack url)

-- | Navigate to the previous page in the browser history.
--   Equivalent to clicking the browser's back button.
goBack :: IO ()
goBack = js_goBack

-- | Navigate to the next page in the browser history.
--   Equivalent to clicking the browser's forward button.
goForward :: IO ()
goForward = js_goForward
