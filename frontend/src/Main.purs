module Main where

import Prelude

import Affjax.RequestBody as Affjax.RequestBody
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Affjax.Web as Affjax
import Data.Argonaut.Core as Argonaut
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Function ((>>>))
import Data.Functor ((<#>))
import Data.HTTP.Method as HTTP.Method
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (error)
import Effect.Ref as Ref
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.DOMTokenList as DOM.DOMTokenList
import Web.DOM.Document as DOM.Document
import Web.DOM.Element (Element)
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




makeSupersedableDelayedAction
  :: forall a. Show a => Milliseconds
  -> (a -> Aff Unit)
  -> Effect (a -> Effect Unit)
makeSupersedableDelayedAction delay action = do
  state <- Ref.new
    { delayCounter: 0
    , actionInProgress: false
    , pendingPayload: Nothing
    }
  pure $ \payload -> do
    Console.log $ "schedule delayed action: payload = " <> show payload
    Ref.modify_ (Record.modify (Proxy :: Proxy "delayCounter") (_ + 1)) state
    counter <- Ref.read state <#> _.delayCounter
    Aff.launchAff_ $ do
      liftEffect $ Console.log $ "launched Aff: counter = " <> show counter
      Aff.delay delay
      liftEffect $ Console.log $ "resuming Aff: counter = " <> show counter
      liftEffect $ do
        superseded <- Ref.read state <#> _.delayCounter >>> (_ /= counter)
        Console.log $ "for Aff with counter " <> show counter <> ": superseded = " <> show superseded
        if superseded then pure unit else Ref.read state <#> _.actionInProgress >>= if _
          then do
            Console.log $ "action is in progress, storing pending payload: " <> show payload
            Ref.modify_ (Record.set (Proxy :: Proxy "pendingPayload") (Just payload)) state
          else do
            let setInProgress b = Ref.modify_ (Record.set (Proxy :: Proxy "actionInProgress") b) state
                callback result = do
                  setInProgress false
                  case result of
                    Left error -> Console.errorShow error
                    Right _ -> Ref.read state <#> _.pendingPayload >>= case _ of
                      Nothing -> pure unit
                      Just payload -> run payload
                run payload = do
                  setInProgress true
                  Aff.runAff_ callback $ action payload
            run payload







undefined = unsafeCoerce unit

data Configuration = Configuration
  { 
  }

main :: Effect Unit
main = do
  Console.log "Skyscope 🔭"
  w <- HTML.window
  el <- Event.EventTarget.eventListener onload
  Event.EventTarget.addEventListener HTML.Event.EventTypes.load el false (HTML.Window.toEventTarget w)


onload :: Event -> Effect Unit
onload e = do
  b <- HTML.HTMLDocument.body =<< HTML.Window.document =<< HTML.window
  case b of
    Just body -> do
      Console.log "found body"
      searchBox <- createSearchBox
      DOM.Node.appendChild (DOM.Element.toNode searchBox) (HTML.HTMLElement.toNode body)
      div <- createElement "div" "" Nothing
      DOM.Node.setTextContent "hello!" $ DOM.Element.toNode div
      DOM.Node.appendChild (DOM.Element.toNode div) (HTML.HTMLElement.toNode body)

      delayedAction <- makeSupersedableDelayedAction (Milliseconds 1000.0) $ \payload -> do
--        result <- Affjax.request $ Affjax.defaultRequest
--          { method = Left HTTP.Method.POST
--          , url = "/find"
--          }
        result <- Affjax.post Affjax.ResponseFormat.json "/find" $
          Just $ Affjax.RequestBody.json $ Argonaut.fromString payload
        liftEffect $ case result of
          Left error -> Console.error $ Affjax.printError error
          Right response -> Console.logShow response.status

      let onclick e = do
            Console.log "click!"
            delayedAction "%rules_lua%.bzl%"
            
      el <- Event.EventTarget.eventListener onclick
      Event.EventTarget.addEventListener HTML.Event.EventTypes.click el false (HTML.HTMLElement.toEventTarget body)

    Nothing -> Console.log "no body"
  Console.log "END"



createElement :: String -> String -> Maybe Element -> Effect Element
createElement name id parent = do
  doc <- HTML.Window.document =<< HTML.window
  element <- DOM.Document.createElement name $ HTML.HTMLDocument.toDocument doc
  DOM.Element.setId id element
  for_ parent $ appendElement element
  pure element

appendElement :: Element -> Element -> Effect Unit
appendElement element parent = do
  DOM.Node.appendChild (DOM.Element.toNode element) (DOM.Element.toNode parent)

addClass :: String -> Element -> Effect Unit
addClass className element = do
  classList <- DOM.Element.classList element
  DOM.DOMTokenList.add classList className

createSearchBox :: Effect Element
createSearchBox = do
  overlay <- createElement "div" "SearchBoxOverlay" Nothing
  searchBox <- createElement "div" "SearchBox" $ Just overlay
  searchBar <- createElement "div" "SearchBar" $ Just searchBox
  patternInput <- createElement "input" "PatternInput" $ Just searchBar
  let title = "The search pattern is a matched against SkyValues using SQLite LIKE."
      placeholder = "Enter a search pattern here to find and display nodes "
                 <> "in the Skyframe graph (you may use % as a wildcard)"
  DOM.Element.setAttribute "placeholder" placeholder patternInput
  DOM.Element.setAttribute "title" title patternInput
  nodeCount <- createElement "span" "NodeCount" $ Just searchBar
  DOM.Node.setTextContent "99,999" $ DOM.Element.toNode nodeCount -- remove later


  --appendElement nodeCount searchBar


  --searchResults <- createElement "div"
  --DOM.Node.setId "SearchResults" searchResults

  --searchInstruction <- createElement "div"
  --DOM.Element.setId "SearchInstruction" searchInstruction 
  --appendElement searchInstruction search 
  



  pure overlay

