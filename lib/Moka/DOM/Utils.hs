{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}


{- |
Module      : Moka.DOM.Utils
Description : Utility functions and type classes for Moka DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module provides utility functions and type classes for working with JavaScript values
in the context of the Moka DOM library. It includes functionality for console logging and
converting Haskell values to JavaScript values.
-}

module Moka.DOM.Utils
  ( consoleLog
  , ToJSVal(..)
  , ToJSValBody(..)
  ) where

import Moka.DOM.FFI.Utils_FFI(js_consoleLog)
import GHC.JS.Prim
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Aeson (ToJSON, encode, Value)
import qualified Data.ByteString.Lazy as BL

-- | Log a message to the JavaScript console.
consoleLog :: Text -> IO ()
consoleLog message = js_consoleLog (toJSString $ T.unpack message)

-- | Represents the detail of a custom event in JavaScript.
type EventDetail = Value

-- | A type class for converting Haskell values to JavaScript values.
class ToJSVal a where
  -- | Convert a Haskell value to a JavaScript value.
  toJSVal :: a -> JSVal

-- | Instance for converting Text to JSVal.
instance ToJSVal Text where
  toJSVal = toJSString . T.unpack

-- | Instance for converting String to JSVal.
instance ToJSVal String where
  toJSVal = toJSString

-- | Instance for converting Int to JSVal.
instance ToJSVal Int where
  toJSVal = toJSString . show

-- | Instance for converting Double to JSVal.
instance ToJSVal Double where
  toJSVal = toJSString . show

-- | Instance for converting Bool to JSVal.
instance ToJSVal Bool where
  toJSVal = toJSString . show

-- | A type class for converting Haskell values to JavaScript object bodies.
class ToJSValBody a where
  -- | Convert a Haskell value to a JavaScript object body.
  toJSValBody :: a -> JSVal

-- | Instance for converting EventDetail to JSVal.
instance ToJSVal EventDetail where
  toJSVal = toJSValBody

-- | Instance for converting any ToJSON instance to JSValBody.
instance ToJSON a => ToJSValBody a where
  toJSValBody = toJSString . T.unpack . TE.decodeUtf8 . BL.toStrict . encode



