{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module      : Moka.DOM.FFI.Events_FFI
Description : Foreign Function Interface for event handling in Moka DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module provides the Foreign Function Interface (FFI) declarations for
event handling functions used in the Moka DOM library.
-}
module Moka.DOM.FFI.Events_FFI where

import GHC.JS.Foreign.Callback
import GHC.JS.Prim

-- | Foreign import for adding an event listener with data.
foreign import javascript unsafe 
  "((eventName, identifier, f) => { document.addEventListener(eventName, (event) => { if (!identifier || event.target.dataset.eventId === identifier) { const detail = event.target.dataset; f(JSON.stringify({event: event, detail: detail})); } }); })"
  js_addEventListenerWithData :: JSVal -> JSVal -> Callback (JSVal -> IO ()) -> IO ()

-- | Foreign import for emitting a custom event.
foreign import javascript unsafe 
  "((eventName, detail) => { const event = new CustomEvent(eventName, { bubbles: true, detail: detail }); document.dispatchEvent(event); })"
  js_emitEvent :: JSVal -> JSVal -> IO ()
