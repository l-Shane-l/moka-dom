{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module      : Moka.DOM.FFI.Networking_FFI
Description : Foreign Function Interface for AJAX networking in Moka DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module provides the Foreign Function Interface (FFI) declarations for
AJAX networking functions used in the Moka DOM library. These functions
wrap the browser's Fetch API to provide AJAX functionality.
-}
module Moka.DOM.FFI.Networking_FFI where

import GHC.JS.Foreign.Callback
import GHC.JS.Prim

-- | Foreign import for GET request.
foreign import javascript unsafe
  "((url, onSuccess, onError) => { fetch(url).then(response => response.text()).then(text => onSuccess(text)).catch(error => onError(error.toString())); })"
  js_ajaxGet :: JSVal -> Callback (JSVal -> IO ()) -> Callback (JSVal -> IO ()) -> IO ()

-- | Foreign import for POST request.
foreign import javascript unsafe
  "((url, body, onSuccess, onError) => { fetch(url, {method: 'POST', body: JSON.stringify(body), headers: {'Content-Type': 'application/json'}}).then(response => response.text()).then(text => onSuccess(text)).catch(error => onError(error.toString())); })"
  js_ajaxPost :: JSVal -> JSVal -> Callback (JSVal -> IO ()) -> Callback (JSVal -> IO ()) -> IO ()

-- | Foreign import for PUT request.
foreign import javascript unsafe
  "((url, body, onSuccess, onError) => { fetch(url, {method: 'PUT', body: JSON.stringify(body), headers: {'Content-Type': 'application/json'}}).then(response => response.text()).then(text => onSuccess(text)).catch(error => onError(error.toString())); })"
  js_ajaxPut :: JSVal -> JSVal -> Callback (JSVal -> IO ()) -> Callback (JSVal -> IO ()) -> IO ()

-- | Foreign import for DELETE request.
foreign import javascript unsafe
  "((url, onSuccess, onError) => { fetch(url, {method: 'DELETE'}).then(response => response.text()).then(text => onSuccess(text)).catch(error => onError(error.toString())); })"
  js_ajaxDelete :: JSVal -> Callback (JSVal -> IO ()) -> Callback (JSVal -> IO ()) -> IO ()
