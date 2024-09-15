{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Moka.DOM.Cookies
Description : Cookie manipulation functions for Moka DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module provides functions for working with browser cookies, including
getting all cookies, setting and retrieving specific cookies, and deleting cookies.
-}
module Moka.DOM.Cookies
  ( getAllCookies
  , getCookie
  , setCookie
  , deleteCookie
  ) where

import Moka.DOM.FFI.Cookies_FFI(js_getCookie, js_setCookie, js_deleteCookie, js_getAllCookies)
import GHC.JS.Prim
import Moka.DOM.Utils
import Data.Text (Text)
import qualified Data.Text as T

-- | Get all cookies as a single Text string.
getAllCookies :: IO Text
getAllCookies = do
  cookies <- js_getAllCookies
  return $ T.pack $ fromJSString cookies

-- | Set a cookie with a name, value, and expiration time in days.
--
--   @param name  The name of the cookie
--   @param value The value to store in the cookie
--   @param days  The number of days until the cookie expires
setCookie :: Text -> Text -> Int -> IO ()
setCookie name value days =
  js_setCookie (toJSVal name) (toJSVal value) days

-- | Get a specific cookie by name.
--   Returns 'Nothing' if the cookie doesn't exist.
getCookie :: Text -> IO (Maybe Text)
getCookie name = do
  value <- js_getCookie (toJSVal name)
  if isNull value
    then return Nothing
    else return $ Just $ T.pack $ fromJSString value

-- | Delete a cookie by name.
deleteCookie :: Text -> IO ()
deleteCookie name = js_deleteCookie (toJSVal name)
