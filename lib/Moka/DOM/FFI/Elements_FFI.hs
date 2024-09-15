{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module      : Moka.DOM.FFI.Elements_FFI
Description : Foreign Function Interface for DOM element manipulation in Moka DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module provides the Foreign Function Interface (FFI) declarations for
DOM element manipulation functions used in the Moka DOM library.
-}
module Moka.DOM.FFI.Elements_FFI where

import GHC.JS.Prim

-- | Foreign import for creating a new DOM element.
foreign import javascript unsafe "((tag) => { return document.createElement(tag); })"
  js_createElement :: JSVal -> IO JSVal

-- | Foreign import for appending a child element to a parent element.
foreign import javascript unsafe "((parent, child) => { if (parent && child) parent.appendChild(child); })"
  js_appendChild :: JSVal -> JSVal -> IO ()

-- | Foreign import for removing an element from the DOM.
foreign import javascript unsafe "((el) => { if (el && el.parentNode) el.parentNode.removeChild(el); })"
  js_removeElement :: JSVal -> IO ()

-- | Foreign import for appending HTML content to the document body.
foreign import javascript unsafe "((html) => { document.body.innerHTML += html; })"
  js_appendToBody :: JSVal -> IO ()

-- | Foreign import for getting an element by its ID.
foreign import javascript unsafe "((id) => { return document.getElementById(id); })"
  js_getElementById :: JSVal -> IO JSVal
