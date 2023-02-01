module Main where

import Prelude

import Affjax.RequestBody as Affjax.RequestBody
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Affjax.Web as Affjax
import Data.Argonaut.Core as Argonaut
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Function ((>>>))
import Data.Functor ((<#>))
import Data.HTTP.Method as HTTP.Method
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.AVar as Effect.AVar
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (error)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign.Object (Object)
import Foreign.Object as Object
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.DOMTokenList as DOM.DOMTokenList
import Web.DOM.Document as DOM.Document
import Web.DOM.Element (Element)
import Web.DOM.Element as DOM.Element
import Web.DOM.HTMLCollection as DOM.HTMLCollection
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
      graph <- createElement "div" "graph" Nothing
      DOM.Node.appendChild (DOM.Element.toNode graph) (HTML.HTMLElement.toNode body)
      graphConfiguration <- makeGraphConfiguration graph
      searchBox <- createSearchBox graphConfiguration
      DOM.Node.appendChild (DOM.Element.toNode searchBox) (HTML.HTMLElement.toNode body)
      div <- createElement "div" "" Nothing
      DOM.Node.setTextContent "hello!" $ DOM.Element.toNode div
      DOM.Node.appendChild (DOM.Element.toNode div) (HTML.HTMLElement.toNode body)
      let payload = "%Main.purs%"

      delayedAction <- makeSupersedableAction $ do
        result <- Affjax.post Affjax.ResponseFormat.json "/find" $
          Just $ Affjax.RequestBody.json $ Argonaut.fromString payload
        liftEffect $ case result of
          Left error -> Aff.throwError $ Aff.error $ Affjax.printError error
          Right response -> response.body <$ Console.logShow response.status

      let onclick e = do
            Console.log $ "click!"
            flip Aff.runAff_ delayedAction $ case _ of
              Left error -> Console.warnShow error
              Right value -> Console.log $ "result: " <> Argonaut.stringify value
            
      el <- Event.EventTarget.eventListener onclick
      Event.EventTarget.addEventListener HTML.Event.EventTypes.click el false (HTML.HTMLElement.toEventTarget body)

    Nothing -> Console.log "no body"
  Console.log "END"


removeAllChildren :: Node -> Effect Unit
removeAllChildren node =
  DOM.Node.firstChild node >>= case _ of
    Nothing -> pure unit
    Just child -> do
      DOM.Node.removeChild child node
      removeAllChildren node

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


data NodeState = Collapsed | Expanded

instance Show NodeState where
  show Collapsed = "Collapsed"
  show Expanded = "Expanded"

type NodeHash = String

type GraphConfiguration =
  { show :: NodeHash -> Maybe NodeState -> Effect Unit
  , hide :: NodeHash -> Effect Unit
  , hidden :: NodeHash -> Effect Boolean
  }

makeGraphRenderer :: Element -> Ref (Object (Maybe NodeState)) -> Effect (Aff Element)
makeGraphRenderer graph nodeStates = makeSupersedableAction $ do
  nodeStates <- liftEffect $ Ref.read nodeStates
  result <- Affjax.post Affjax.ResponseFormat.document "/render" $
    Just $ Affjax.RequestBody.json $ Argonaut.fromObject $
      nodeStates <#> case _ of
        Just Collapsed -> Argonaut.jsonTrue
        Just Expanded -> Argonaut.jsonFalse
        Nothing -> Argonaut.jsonNull
  liftEffect $ case result of
    Left error -> Aff.throwError $ Aff.error $ Affjax.printError error
    Right response -> do
      svg <- DOM.HTMLCollection.item 0 =<< DOM.Document.getElementsByTagName "svg" response.body
      case svg of
        Nothing -> Aff.throwError $ Aff.error "svg element not found"
        Just svg -> pure svg

decorateGraph :: GraphConfiguration -> Element -> Effect Unit
decorateGraph graphConfiguration svg = pure unit

makeGraphConfiguration :: Element -> Effect GraphConfiguration
makeGraphConfiguration graph = do
  nodeStates <- Ref.new Object.empty
  renderGraph <- makeGraphRenderer graph nodeStates
  let show hash state = Ref.modify_ (Object.insert hash state) nodeStates *> updateGraph
      hide hash = Ref.modify_ (Object.delete hash) nodeStates *> updateGraph
      hidden hash = (hash `Object.member` _) <$> Ref.read nodeStates
      updateGraph = runAff $ renderGraph >>= \svg -> liftEffect $ do
        decorateGraph { show, hide, hidden } svg
        removeAllChildren $ DOM.Element.toNode graph
        appendElement svg graph
  pure { show, hide, hidden }


type NodeSelection =
  { select :: Boolean -> NodeHash -> Effect Unit
  , selected :: NodeHash -> Effect Boolean
  , selection :: Effect (Array NodeHash)
  }

makeNodeSelection :: Effect NodeSelection
makeNodeSelection = undefined

createSearchBox :: GraphConfiguration -> Effect Element
createSearchBox graphConfiguration = do
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

  graphConfiguration.show "001678e06f8e85e15249e54921ac2c3677f5f6ba783ea8cbf1f6dc51bbc117e7" (Just Expanded)
  graphConfiguration.show "0019dbccab2baa43717ae7c3cc24c75b2042dec45ad8bebc1c18d903db36f435" (Just Expanded)
  graphConfiguration.show "003927b7b30435abd8b50c7c84e5e657b75a710c116bfba804ed9611c6affff5" (Just Expanded)





  --appendElement nodeCount searchBar


  --searchResults <- createElement "div"
  --DOM.Node.setId "SearchResults" searchResults

  --searchInstruction <- createElement "div"
  --DOM.Element.setId "SearchInstruction" searchInstruction 
  --appendElement searchInstruction search 
  



  pure overlay


makeSupersedableAction :: forall a. Aff a -> Effect (Aff a)
makeSupersedableAction action = do
  mutex <- Effect.AVar.new unit
  tokenRef <- Ref.new 0
  pure $ do
    token <- liftEffect $ Ref.modify (_ + 1) tokenRef
    let run = liftEffect (Ref.read tokenRef) >>= \t -> if t == token
          then action else Aff.throwError $ Aff.error "superseded"
    Aff.bracket (AVar.take mutex) (flip AVar.put mutex) (const run)

runAff :: Aff Unit -> Effect Unit
runAff = Aff.runAff_ $ case _ of
  Left error -> Console.warnShow error
  Right _ -> pure unit
