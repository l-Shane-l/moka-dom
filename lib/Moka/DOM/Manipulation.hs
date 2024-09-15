{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Moka.DOM.Manipulation
Description : DOM manipulation functions for Moka DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module provides functions for manipulating DOM elements, including
setting and getting attributes, modifying element content, and querying
the DOM.
-}
module Moka.DOM.Manipulation
  ( setInnerHTML
  , getInnerHTML
  , setAttribute
  , getAttribute
  , addClass
  , removeClass
  , querySelector
  , getTextContent
  , setTextContent
  ) where

import Moka.DOM.FFI.Manipulation_FFI(js_getAttribute, js_setAttribute, js_setInnerHTML, js_getInnerHTML, js_querySelector, js_setTextContent, js_getTextContent, js_addClass, js_removeClass)
import Moka.DOM.Elements (Element(..))
import GHC.JS.Prim
import Data.Text (Text)
import qualified Data.Text as T

-- | Set the inner HTML of an element.
setInnerHTML :: Element -> Text -> IO ()
setInnerHTML (Element el) html = js_setInnerHTML el (toJSString $ T.unpack html)

-- | Get the inner HTML of an element.
getInnerHTML :: Element -> IO Text
getInnerHTML (Element el) = do
  content <- js_getInnerHTML el
  return $ T.pack $ fromJSString content

-- | Set an attribute on an element.
setAttribute :: Element -> Text -> Text -> IO ()
setAttribute (Element el) attr value = js_setAttribute el (toJSString $ T.unpack attr) (toJSString $ T.unpack value)

-- | Get the value of an attribute from an element.
--   Returns 'Nothing' if the attribute doesn't exist.
getAttribute :: Element -> Text -> IO (Maybe Text)
getAttribute (Element el) attr = do
  value <- js_getAttribute el (toJSString $ T.unpack attr)
  return $
    if isNull value
      then Nothing
      else Just (T.pack $ fromJSString value)

-- | Add a class to an element.
addClass :: Element -> Text -> IO ()
addClass (Element el) className = js_addClass el (toJSString $ T.unpack className)

-- | Remove a class from an element.
removeClass :: Element -> Text -> IO ()
removeClass (Element el) className = js_removeClass el (toJSString $ T.unpack className)

-- | Query the DOM for an element using a CSS selector.
--   Returns 'Nothing' if no element is found.
querySelector :: Text -> IO (Maybe Element)
querySelector selector = do
  el <- js_querySelector (toJSString $ T.unpack selector)
  return $ if isNull el then Nothing else Just (Element el)

-- | Get the text content of an element.
getTextContent :: Element -> IO Text
getTextContent (Element el) = do
  content <- js_getTextContent el
  return $ T.pack $ fromJSString content

-- | Set the text content of an element.
setTextContent :: Element -> Text -> IO ()
setTextContent (Element el) text = js_setTextContent el (toJSString $ T.unpack text)
