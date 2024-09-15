{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module      : Moka.DOM.FFI.Manipulation_FFI
Description : Foreign Function Interface for DOM manipulation in Moka DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module provides the Foreign Function Interface (FFI) declarations for
DOM manipulation functions used in the Moka DOM library.
-}
module Moka.DOM.FFI.Manipulation_FFI where

import GHC.JS.Prim

-- | Foreign import for setting an attribute on an element.
foreign import javascript unsafe "((el, attr, value) => { if (el) el.setAttribute(attr, value); })"
  js_setAttribute :: JSVal -> JSVal -> JSVal -> IO ()

-- | Foreign import for getting an attribute from an element.
foreign import javascript unsafe "((el, attr) => { return el ? el.getAttribute(attr) : null; })"
  js_getAttribute :: JSVal -> JSVal -> IO JSVal

-- | Foreign import for adding a class to an element.
foreign import javascript unsafe "((el, className) => { if (el) el.classList.add(className); })"
  js_addClass :: JSVal -> JSVal -> IO ()

-- | Foreign import for removing a class from an element.
foreign import javascript unsafe "((el, className) => { if (el) el.classList.remove(className); })"
  js_removeClass :: JSVal -> JSVal -> IO ()

-- | Foreign import for setting the inner HTML of an element.
foreign import javascript unsafe "((el, html) => { if (el) el.innerHTML = html; })"
  js_setInnerHTML :: JSVal -> JSVal -> IO ()

-- | Foreign import for getting the inner HTML of an element.
foreign import javascript unsafe "((el) => { return el ? el.innerHTML : null; })"
  js_getInnerHTML :: JSVal -> IO JSVal

-- | Foreign import for setting the text content of an element.
foreign import javascript unsafe "((el, text) => { if (el) el.textContent = text; })"
  js_setTextContent :: JSVal -> JSVal -> IO ()

-- | Foreign import for getting the text content of an element.
foreign import javascript unsafe "((el) => { return el ? el.textContent : null; })"
  js_getTextContent :: JSVal -> IO JSVal

-- | Foreign import for querying the DOM with a CSS selector.
foreign import javascript unsafe "((selector) => { return document.querySelector(selector); })"
  js_querySelector :: JSVal -> IO JSVal
