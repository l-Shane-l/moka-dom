{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module      : Moka.DOM.FFI.Storage_FFI
Description : Foreign Function Interface for Local Storage manipulation in Moka DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module provides the Foreign Function Interface (FFI) declarations for
Local Storage manipulation functions used in the Moka DOM library.
-}
module Moka.DOM.FFI.Storage_FFI where

import GHC.JS.Prim

-- | Foreign import for retrieving an item from Local Storage.
foreign import javascript unsafe "((key) => { return localStorage.getItem(key); })"
  js_getLocalStorageItem :: JSVal -> IO JSVal

-- | Foreign import for setting an item in Local Storage.
foreign import javascript unsafe "((key, value) => { localStorage.setItem(key, value); })"
  js_setLocalStorageItem :: JSVal -> JSVal -> IO ()

-- | Foreign import for removing an item from Local Storage.
foreign import javascript unsafe "((key) => { localStorage.removeItem(key); })"
  js_removeLocalStorageItem :: JSVal -> IO ()
