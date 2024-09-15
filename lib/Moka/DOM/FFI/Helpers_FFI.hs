{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module      : Moka.DOM.FFI.Helpers_FFI
Description : Foreign Function Interface for helper functions in Moka DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module provides the Foreign Function Interface (FFI) declarations for
helper functions used in the Moka DOM library.
-}
module Moka.DOM.FFI.Helpers_FFI where

import GHC.JS.Prim

-- | Foreign import for getting window dimensions.
foreign import javascript unsafe
  "(() => { return JSON.stringify({width: window.innerWidth, height: window.innerHeight}); })"
  js_getWindowDimensions :: IO JSVal

-- | Foreign import for scrolling to an element.
foreign import javascript unsafe
  "((el) => { if (el) el.scrollIntoView({behavior: 'smooth', block: 'start'}); })"
  js_scrollToElement :: JSVal -> IO ()

-- | Foreign import for setting document title.
foreign import javascript unsafe
  "((title) => { document.title = title; })"
  js_setDocumentTitle :: JSVal -> IO ()

-- | Foreign import for getting current timestamp.
foreign import javascript unsafe
  "(() => { return Date.now(); })"
  js_getCurrentTimestamp :: IO Int

-- | Foreign import for parsing JSON.
foreign import javascript unsafe
  "((jsonString) => { try { return JSON.parse(jsonString); } catch (e) { return null; } })"
  js_parseJSON :: JSVal -> IO JSVal
