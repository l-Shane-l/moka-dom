{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Tests.Events
Description : Tests for event handling functions in Moka-DOM
License     : BSD-3-Clause
Maintainer  : shane@peregrinum.dev

This module contains tests for event handling functions provided by Moka-DOM,
including adding event listeners and testing visual changes triggered by events.
-}
module Tests.Events( 
   testEventListener
 , testVisualChangeOnClick
) where

import Moka.DOM.Events
import Moka.DOM.Manipulation
import Moka.DOM.Elements
import Moka.DOM.Utils
import Tests.Utils(logTestResult)
import qualified Data.Text as T

-- | Test adding and triggering an event listener
testEventListener :: IO ()
testEventListener = do
  maybeButton <- createElement "button"
  case maybeButton of
    Just button -> do
      setInnerHTML button "Click Me"
      setAttribute button "id" "test-button"
      setAttribute button "data-event-id" "test-click-button"
      maybeTestArea <- getElementById "test-area"
      case maybeTestArea of
        Just testArea -> do
          appendElement testArea button
          addEventListener "click" (Just "test-click-button") $ \_ -> do
            consoleLog "Button clicked!"
            logTestResult "Event Listener" "Passed (Check console for click event)"
          logTestResult "Event Listener" "Test setup complete. Please click the 'Click Me' button."
        Nothing -> logTestResult "Event Listener" "Failed (Test area not found)"
    Nothing -> logTestResult "Event Listener" "Failed (Button creation failed)"

-- | Test visual changes triggered by a click event
testVisualChangeOnClick :: IO ()
testVisualChangeOnClick = do
  maybeButton <- createElement "button"
  maybeTargetDiv <- createElement "div"
  case (maybeButton, maybeTargetDiv) of
    (Just button, Just targetDiv) -> do
      setInnerHTML button "Change Color"
      setAttribute button "id" "change-color-button"
      setAttribute button "data-event-id" "color-change-button"
      
      setInnerHTML targetDiv "Click the button to change my color"
      setAttribute targetDiv "id" "color-change-target"
      setAttribute targetDiv "style" "padding: 10px; border: 1px solid black; background-color: white;"
      
      maybeTestArea <- getElementById "test-area"
      case maybeTestArea of
        Just testArea -> do
          appendElement testArea button
          appendElement testArea targetDiv
          
          addEventListener "click" (Just "color-change-button") $ \_ -> do
            consoleLog "Color change button clicked!"
            maybeTarget <- getElementById "color-change-target"
            case maybeTarget of
              Just target -> do
                currentStyle <- getAttribute target "style"
                case currentStyle of
                  Just style ->
                    if "background-color: white" `T.isInfixOf` style
                      then setAttribute target "style" "padding: 10px; border: 1px solid black; background-color: yellow;"
                      else setAttribute target "style" "padding: 10px; border: 1px solid black; background-color: white;"
                  Nothing -> setAttribute target "style" "padding: 10px; border: 1px solid black; background-color: yellow;"
                logTestResult "Visual Change" "Color changed successfully"
              Nothing -> logTestResult "Visual Change" "Failed to find target element"
          
          logTestResult "Visual Change" "Test setup complete. Please click the 'Change Color' button to test."
        Nothing -> logTestResult "Visual Change" "Failed to find test area"
    _ -> logTestResult "Visual Change" "Failed to create elements for visual change test"
