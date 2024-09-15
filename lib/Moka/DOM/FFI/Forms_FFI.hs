{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module      : Moka.DOM.FFI.Forms_FFI
Description : Foreign Function Interface for form handling in Moka DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module provides the Foreign Function Interface (FFI) declarations for
form handling functions used in the Moka DOM library.
-}
module Moka.DOM.FFI.Forms_FFI where

import GHC.JS.Prim

-- | Foreign import for submitting a form.
foreign import javascript unsafe
  "((formEl) => { if (formEl && formEl.tagName === 'FORM') formEl.submit(); })"
  js_submitForm :: JSVal -> IO ()

-- | Foreign import for resetting a form.
foreign import javascript unsafe
  "((formEl) => { if (formEl && formEl.tagName === 'FORM') formEl.reset(); })"
  js_resetForm :: JSVal -> IO ()

-- | Foreign import for checking form validity.
foreign import javascript unsafe
  "((formEl) => { return (formEl && formEl.tagName === 'FORM') ? formEl.checkValidity() : false; })"
  js_checkFormValidity :: JSVal -> IO Bool

-- | Foreign import for getting all form data as a JSON object.
foreign import javascript unsafe
  "((formEl) => { if (formEl && formEl.tagName === 'FORM') { const formData = new FormData(formEl); return JSON.stringify(Object.fromEntries(formData)); } return '{}'; })"
  js_getFormData :: JSVal -> IO JSVal

-- | Foreign import for getting the value of a form element.
foreign import javascript unsafe "((el) => { return el ? el.value : null; })"
  js_getValue :: JSVal -> IO JSVal

-- | Foreign import for setting the value of a form element.
foreign import javascript unsafe "((el, value) => { if (el) el.value = value; })"
  js_setValue :: JSVal -> JSVal -> IO ()
