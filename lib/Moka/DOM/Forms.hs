{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Moka.DOM.Forms
Description : Form handling functions for Moka DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module provides functions for interacting with HTML forms, including
getting and setting form element values, submitting and resetting forms,
checking form validity, and retrieving form data.
-}
module Moka.DOM.Forms
  ( getValue
  , setValue
  , submitForm
  , resetForm
  , checkFormValidity
  , getFormData
  ) where

import Moka.DOM.FFI.Forms_FFI(js_getValue, js_setValue, js_getFormData, js_checkFormValidity, js_resetForm, js_submitForm)
import Moka.DOM.Elements (Element(..))
import GHC.JS.Prim
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Aeson (Value, decode)
import qualified Data.ByteString.Lazy as BL

-- | Get the value of a form element.
getValue :: Element -> IO Text
getValue (Element el) = do
  value <- js_getValue el
  return $ T.pack $ fromJSString value

-- | Set the value of a form element.
setValue :: Element -> Text -> IO ()
setValue (Element el) value = js_setValue el (toJSString $ T.unpack value)

-- | Submit a form.
--   This function has no effect if the element is not a form.
submitForm :: Element -> IO ()
submitForm (Element el) = js_submitForm el

-- | Reset a form to its initial state.
--   This function has no effect if the element is not a form.
resetForm :: Element -> IO ()
resetForm (Element el) = js_resetForm el

-- | Check if a form is valid according to HTML5 form validation rules.
--   Returns 'False' if the element is not a form.
checkFormValidity :: Element -> IO Bool
checkFormValidity (Element el) = js_checkFormValidity el

-- | Get all form data as a JSON object.
--   Returns 'Nothing' if the element is not a form or if the data couldn't be parsed.
getFormData :: Element -> IO (Maybe Value)
getFormData (Element el) = do
  jsonStr <- js_getFormData el
  let textStr = T.pack $ fromJSString jsonStr
  return $ decode $ BL.fromStrict $ TE.encodeUtf8 textStr
