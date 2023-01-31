module Main where

import Prelude

import Web.Event.Event (Event)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Web.Event.EventTarget (addEventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (load)
import Web.HTML.HTMLDocument (body)
import Web.HTML.Window (document, toEventTarget)
import Web.Event.EventTarget (EventListener, eventListener)

main :: Effect Unit
main = do
  log "Skyscope 🔭"
  w <- window
  el <- eventListener onload
  addEventListener load el false (toEventTarget w)

onload :: Event -> Effect Unit
onload e = do
  b <- body =<< document =<< window
  case b of
    Just _ -> log "found body"
    Nothing -> log "no body"
  log "END"
