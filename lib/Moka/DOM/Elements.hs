{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Moka.DOM.Elements
Description : DOM element manipulation functions for Moka DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module provides functions for creating, selecting, and manipulating DOM elements.
It includes functionality for appending elements to the body, creating new elements,
selecting elements by ID, and removing elements from the DOM.
-}
module Moka.DOM.Elements
  ( Element(..)
  , appendToBody
  , createElement
  , getElementById
  , removeElement
  , appendElement
  ) where

import Moka.DOM.FFI.Elements_FFI(js_getElementById, js_appendToBody, js_appendChild, js_createElement, js_removeElement)
import GHC.JS.Prim
import Data.Text (Text)
import qualified Data.Text as T

-- | Represents a DOM element.
newtype Element = Element { unElement :: JSVal }

-- | Append HTML content to the end of the document body.
appendToBody :: Text -> IO ()
appendToBody html = js_appendToBody (toJSString $ T.unpack html)

-- | Create a new DOM element.
--   Returns 'Nothing' if the element couldn't be created.
createElement :: Text -> IO (Maybe Element)
createElement tag = do
  el <- js_createElement (toJSString $ T.unpack tag)
  return $ if isNull el then Nothing else Just (Element el)

-- | Get an element by its ID.
--   Returns 'Nothing' if no element with the given ID exists.
getElementById :: Text -> IO (Maybe Element)
getElementById elementId = do
  element <- js_getElementById (toJSString $ T.unpack elementId)
  return $ if isNull element then Nothing else Just (Element element)

-- | Remove an element from the DOM.
removeElement :: Element -> IO ()
removeElement (Element el) = js_removeElement el

-- | Append a child element to a parent element.
appendElement :: Element  -- ^ Parent element
              -> Element  -- ^ Child element to append
              -> IO ()
appendElement (Element parent) (Element child) = js_appendChild parent child
