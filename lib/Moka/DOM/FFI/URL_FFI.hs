{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module      : Moka.DOM.FFI.URL_FFI
Description : Foreign Function Interface for URL manipulation in Moka DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module provides the Foreign Function Interface (FFI) declarations for
URL and history manipulation functions used in the Moka DOM library.
-}
module Moka.DOM.FFI.URL_FFI where

import GHC.JS.Prim

-- | Foreign import for getting the current URL.
foreign import javascript unsafe "(() => { return window.location.href; })"
  js_getCurrentURL :: IO JSVal

-- | Foreign import for pushing a new state to the browser history.
foreign import javascript unsafe "((url) => { window.history.pushState({}, '', url); })"
  js_pushState :: JSVal -> IO ()

-- | Foreign import for navigating back in the browser history.
foreign import javascript unsafe "(() => { window.history.back(); })"
  js_goBack :: IO ()

-- | Foreign import for navigating forward in the browser history.
foreign import javascript unsafe "(() => { window.history.forward(); })"
  js_goForward :: IO ()
