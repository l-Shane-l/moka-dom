{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Moka.DOM.Events
Description : Event handling functions for Moka DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module provides functions for handling DOM events, including adding event listeners
and emitting custom events. It also defines types for event callbacks and event details.
-}
module Moka.DOM.Events
  ( addEventListener
  , emitEvent
  , EventCallback
  , EventDetail
  ) where

import Moka.DOM.FFI.Events_FFI(js_addEventListenerWithData, js_emitEvent)
import Moka.DOM.Utils
import GHC.JS.Foreign.Callback
import GHC.JS.Prim
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.Aeson (decode, Value)
import qualified Data.ByteString.Lazy as BL

-- | Type alias for event callback functions.
--   The callback receives an 'EventDetail' containing event information.
type EventCallback = EventDetail -> IO ()

-- | Type alias for event detail, which is a JSON object.
type EventDetail = Value

-- | Global list to keep callbacks alive.
--   This prevents callbacks from being garbage collected.
{-# NOINLINE callbacks #-}
callbacks :: IORef [Callback (JSVal -> IO ())]
callbacks = unsafePerformIO $ newIORef []

-- | Add a callback to the global list to keep it alive.
addCallback :: Callback (JSVal -> IO ()) -> IO ()
addCallback cb = modifyIORef callbacks (cb :)

-- | Add an event listener to the document.
--
--   @param event      The name of the event to listen for
--   @param identifier An optional identifier to filter events
--   @param callback   The function to call when the event occurs
addEventListener :: Text -> Maybe Text -> EventCallback -> IO ()
addEventListener event identifier callback = do
  cb <- asyncCallback1 $ \jsVal -> do
    let textVal = T.pack $ fromJSString jsVal
        byteStringVal = BL.fromStrict $ TE.encodeUtf8 textVal
    case decode byteStringVal of
      Just detail -> callback detail
      Nothing -> return ()  -- Or handle the error as appropriate
  js_addEventListenerWithData 
    (toJSString $ T.unpack event) 
    (maybe (toJSString "") (toJSString . T.unpack) identifier)
    cb
  addCallback cb

-- | Emit a custom event.
--
--   @param eventName The name of the custom event to emit
--   @param detail    The detail object to include with the event
emitEvent :: Text -> EventDetail -> IO ()
emitEvent eventName detail = 
  js_emitEvent (toJSString $ T.unpack eventName) (toJSVal detail)
