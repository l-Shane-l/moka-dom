{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Moka.DOM.Storage
Description : Local Storage manipulation functions for Moka DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module provides functions for interacting with the browser's Local Storage.
It includes functionality for getting, setting, and removing items from Local Storage.
-}
module Moka.DOM.Storage
  ( getLocalStorageItem
  , setLocalStorageItem
  , removeLocalStorageItem
  ) where

import Moka.DOM.FFI.Storage_FFI(js_removeLocalStorageItem, js_setLocalStorageItem, js_getLocalStorageItem)
import GHC.JS.Prim
import Data.Text (Text)
import qualified Data.Text as T

-- | Retrieve an item from Local Storage by its key.
--
--   Returns 'Nothing' if the item doesn't exist.
getLocalStorageItem :: Text -> IO (Maybe Text)
getLocalStorageItem key = do
  value <- js_getLocalStorageItem (toJSString $ T.unpack key)
  if isNull value
    then return Nothing
    else return $ Just $ T.pack $ fromJSString value

-- | Set an item in Local Storage with the given key and value.
--
--   If an item with the same key already exists, it will be overwritten.
setLocalStorageItem :: Text -> Text -> IO ()
setLocalStorageItem key value =
  js_setLocalStorageItem (toJSString $ T.unpack key) (toJSString $ T.unpack value)

-- | Remove an item from Local Storage by its key.
--
--   If the item doesn't exist, this operation has no effect.
removeLocalStorageItem :: Text -> IO ()
removeLocalStorageItem key = js_removeLocalStorageItem (toJSString $ T.unpack key)
