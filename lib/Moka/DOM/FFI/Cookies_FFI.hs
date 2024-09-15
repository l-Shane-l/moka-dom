{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module      : Moka.DOM.FFI.Cookies_FFI
Description : Foreign Function Interface for cookie manipulation in Moka DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module provides the Foreign Function Interface (FFI) declarations for
cookie manipulation functions used in the Moka DOM library.
-}
module Moka.DOM.FFI.Cookies_FFI where

import GHC.JS.Prim

-- | Foreign import for logging element details (for debugging).
foreign import javascript unsafe 
  "((el) => { console.log('Element:', el, 'tagName:', el ? el.tagName : 'N/A', 'id:', el ? el.id : 'N/A', 'dataset:', el ? el.dataset : 'N/A'); })"
  js_logElement :: JSVal -> IO ()

-- | Foreign import for getting all cookies.
foreign import javascript unsafe 
  "(() => { return document.cookie; })"
  js_getAllCookies :: IO JSVal

-- | Foreign import for setting a cookie.
foreign import javascript unsafe 
  "((name, value, days) => { let expires = ''; if (days) { const date = new Date(); date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000)); expires = '; expires=' + date.toUTCString(); } document.cookie = name + '=' + (value || '') + expires + '; path=/'; })"
  js_setCookie :: JSVal -> JSVal -> Int -> IO ()

-- | Foreign import for getting a specific cookie.
foreign import javascript unsafe 
  "((name) => { const nameEQ = name + '='; const ca = document.cookie.split(';'); for (let i = 0; i < ca.length; i++) { let c = ca[i]; while (c.charAt(0) === ' ') c = c.substring(1, c.length); if (c.indexOf(nameEQ) === 0) return c.substring(nameEQ.length, c.length); } return null; })"
  js_getCookie :: JSVal -> IO JSVal

-- | Foreign import for deleting a cookie.
foreign import javascript unsafe 
  "((name) => { document.cookie = name + '=; Path=/; Expires=Thu, 01 Jan 1970 00:00:01 GMT;'; })"
  js_deleteCookie :: JSVal -> IO ()
