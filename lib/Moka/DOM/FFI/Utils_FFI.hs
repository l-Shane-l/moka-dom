{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module      : Moka.DOM.FFI.Utils_FFI
Description : Foreign Function Interface for Moka DOM utilities
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module provides the Foreign Function Interface (FFI) declarations for
utility functions used in the Moka DOM library.
-}
module Moka.DOM.FFI.Utils_FFI where

import GHC.JS.Prim

-- | Foreign import for logging messages to the JavaScript console.
foreign import javascript unsafe "((message) => { console.log(message); })"
  js_consoleLog :: JSVal -> IO ()
