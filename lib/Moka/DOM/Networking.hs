{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Moka.DOM.Networking
Description : AJAX networking functions for Moka DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module provides functions for making AJAX requests (GET, POST, PUT, DELETE)
using the browser's Fetch API. It includes functionality for handling responses
and errors in a Haskell-friendly way.
-}
module Moka.DOM.Networking
  ( ajaxDelete
  , ajaxPut
  , ajaxPost
  , ajaxGet
  ) where

import Moka.DOM.FFI.Networking_FFI(js_ajaxGet, js_ajaxPut, js_ajaxPost, js_ajaxDelete)
import Moka.DOM.Utils
import GHC.JS.Foreign.Callback
import GHC.JS.Prim
import Data.Aeson (ToJSON) 
import Data.Text (Text)
import qualified Data.Text as T

-- | Type alias for AJAX callback functions.
--   The callback receives 'Just Text' on success, or 'Nothing' on error.
type AjaxCallback = Maybe Text -> IO ()

-- | Perform a GET request.
--
--   @param url      The URL to send the request to
--   @param callback The function to call with the result
ajaxGet :: Text -> AjaxCallback -> IO ()
ajaxGet url callback = do
  successCallback <- asyncCallback1 $ \jsVal -> do
    let result = T.pack $ fromJSString jsVal
    callback (Just result)
  
  errorCallback <- asyncCallback1 $ \jsVal -> do
    let errorMsg = T.pack $ fromJSString jsVal
    callback Nothing
    consoleLog $ "AJAX GET Error: " <> errorMsg
  js_ajaxGet (toJSVal url) successCallback errorCallback

-- | Perform a POST request.
--
--   @param url      The URL to send the request to
--   @param body     The body of the request (will be JSON-encoded)
--   @param callback The function to call with the result
ajaxPost :: ToJSON a => Text -> a -> AjaxCallback -> IO ()
ajaxPost url body callback = do
  successCallback <- asyncCallback1 $ \jsVal -> do
    let result = T.pack $ fromJSString jsVal
    callback (Just result)
  
  errorCallback <- asyncCallback1 $ \jsVal -> do
    let errorMsg = T.pack $ fromJSString jsVal
    callback Nothing
    consoleLog $ "AJAX POST Error: " <> errorMsg
  js_ajaxPost (toJSVal url) (toJSValBody body) successCallback errorCallback

-- | Perform a PUT request.
--
--   @param url      The URL to send the request to
--   @param body     The body of the request (will be JSON-encoded)
--   @param callback The function to call with the result
ajaxPut :: ToJSON a => Text -> a -> AjaxCallback -> IO ()
ajaxPut url body callback = do
  successCallback <- asyncCallback1 $ \jsVal -> do
    let result = T.pack $ fromJSString jsVal
    callback (Just result)
  
  errorCallback <- asyncCallback1 $ \jsVal -> do
    let errorMsg = T.pack $ fromJSString jsVal
    callback Nothing
    consoleLog $ "AJAX PUT Error: " <> errorMsg
  js_ajaxPut (toJSVal url) (toJSValBody body) successCallback errorCallback

-- | Perform a DELETE request.
--
--   @param url      The URL to send the request to
--   @param callback The function to call with the result
ajaxDelete :: Text -> AjaxCallback -> IO ()
ajaxDelete url callback = do
  successCallback <- asyncCallback1 $ \jsVal -> do
    let result = T.pack $ fromJSString jsVal
    callback (Just result)
  
  errorCallback <- asyncCallback1 $ \jsVal -> do
    let errorMsg = T.pack $ fromJSString jsVal
    callback Nothing
    consoleLog $ "AJAX DELETE Error: " <> errorMsg
  js_ajaxDelete (toJSVal url) successCallback errorCallback

