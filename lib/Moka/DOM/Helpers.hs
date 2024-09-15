{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module      : Moka.DOM.Helpers
Description : Helper functions for DOM operations in Moka DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module provides various helper functions for common DOM operations
and utilities, such as getting window dimensions, scrolling to elements,
setting document title, getting timestamps, and parsing JSON.
-}
module Moka.DOM.Helpers
  ( getWindowDimensions
  , scrollToElement
  , setDocumentTitle
  , getCurrentTimestamp
  , parseJSON
  ) where

import Moka.DOM.FFI.Helpers_FFI(js_getWindowDimensions, js_parseJSON, js_getCurrentTimestamp, js_scrollToElement, js_setDocumentTitle)
import Moka.DOM.Utils (ToJSVal(..))
import Moka.DOM.Elements
import GHC.JS.Prim
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Aeson (Value(..), decode, (.:))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BL
import Control.Monad (join)

-- | Get the current dimensions of the browser window.
--   Returns 'Nothing' if the dimensions couldn't be retrieved or parsed.
getWindowDimensions :: IO (Maybe (Int, Int))
getWindowDimensions = do
  jsonStr <- js_getWindowDimensions
  let textStr = T.pack $ fromJSString jsonStr
      maybeValue = decode $ BL.fromStrict $ TE.encodeUtf8 textStr :: Maybe Value
  return $ join $ flip fmap maybeValue $ \case
    Object v -> 
      case parseEither (\obj -> (,) <$> obj .: "width" <*> obj .: "height") v of
        Right (width, height) -> Just (width, height)
        Left _ -> Nothing
    _ -> Nothing

-- | Scroll the page to bring the specified element into view.
scrollToElement :: Element -> IO ()
scrollToElement (Element el) = js_scrollToElement el

-- | Set the title of the current document.
setDocumentTitle :: Text -> IO ()
setDocumentTitle title = js_setDocumentTitle (toJSVal title)

-- | Get the current timestamp (milliseconds since Unix epoch).
getCurrentTimestamp :: IO Int
getCurrentTimestamp = js_getCurrentTimestamp

-- | Parse a JSON string into an Aeson Value.
--   Returns 'Nothing' if the JSON is invalid or couldn't be parsed.
parseJSON :: Text -> IO (Maybe Value)
parseJSON jsonText = do
  result <- js_parseJSON (toJSVal jsonText)
  if isNull result
    then return Nothing
    else do
      let textStr = T.pack $ fromJSString result
      return $ decode $ BL.fromStrict $ TE.encodeUtf8 textStr

