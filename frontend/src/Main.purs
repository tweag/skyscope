module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document as DOM.Document
import Web.DOM.Element (Element)
import Web.DOM.DOMTokenList as DOM.DOMTokenList
import Web.DOM.Element as DOM.Element
import Web.DOM.Node (Node)
import Web.DOM.Node as DOM.Node
import Web.Event.Event (Event)
import Web.Event.EventTarget as Event.EventTarget
import Web.HTML as HTML
import Web.HTML.Event.EventTypes as HTML.Event.EventTypes
import Web.HTML.HTMLDocument as HTML.HTMLDocument
import Web.HTML.HTMLElement as HTML.HTMLElement
import Web.HTML.Window as HTML.Window

undefined = unsafeCoerce unit

data Configuration = Configuration
  { 
  }

main :: Effect Unit
main = do
  log "Skyscope 🔭"
  w <- HTML.window
  el <- Event.EventTarget.eventListener onload
  Event.EventTarget.addEventListener HTML.Event.EventTypes.load el false (HTML.Window.toEventTarget w)


onload :: Event -> Effect Unit
onload e = do
  b <- HTML.HTMLDocument.body =<< HTML.Window.document =<< HTML.window
  case b of
    Just body -> do
      log "found body"
      searchBox <- createSearchBox
      DOM.Node.appendChild (DOM.Element.toNode searchBox) (HTML.HTMLElement.toNode body)
      div <- createElement "div"
      DOM.Node.setTextContent "hello!" $ DOM.Element.toNode div
      DOM.Node.appendChild (DOM.Element.toNode div) (HTML.HTMLElement.toNode body)
    Nothing -> log "no body"
  log "END"



createElement :: String -> Effect Element
createElement name = do
  doc <- HTML.Window.document =<< HTML.window
  DOM.Document.createElement name $ HTML.HTMLDocument.toDocument doc

appendElement :: Element -> Element -> Effect Unit
appendElement element parent = do
  DOM.Node.appendChild (DOM.Element.toNode element) (DOM.Element.toNode parent)

addClass :: String -> Element -> Effect Unit
addClass className element = do
  classList <- DOM.Element.classList element
  DOM.DOMTokenList.add classList className

createSearchBox :: Effect Element
createSearchBox = do
  overlay <- createElement "div"
  DOM.Element.setId "SearchBoxOverlay" overlay
  searchBox <- createElement "div"
  DOM.Element.setId "SearchBox" searchBox
  appendElement searchBox overlay
  searchBar <- createElement "div"
  DOM.Element.setId "SearchBar" searchBar
  appendElement searchBar searchBox
  patternInput <- createElement "input"
  DOM.Element.setId "PatternInput" patternInput
  let title = "The search pattern is a matched against SkyValues using SQLite LIKE."
      placeholder = "Enter a search pattern here to find and display nodes "
                 <> "in the Skyframe graph (you may use % as a wildcard)"
  DOM.Element.setAttribute "placeholder" placeholder patternInput
  DOM.Element.setAttribute "title" title patternInput
  appendElement patternInput searchBar
  nodeCount <- createElement "span"
  DOM.Element.setId "NodeCount" nodeCount
  appendElement nodeCount searchBar
  pure overlay

