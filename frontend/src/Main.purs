module Main where

import Prelude

import Affjax.RequestBody as Affjax.RequestBody
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Affjax.Web as Affjax
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Argonaut
import Data.Array ((!!))
import Data.Bifunctor (lmap)
import Data.Either (Either(..), fromRight)
import Data.Either as Either
import Data.Foldable (for_, traverse_, sequence_)
import Data.Formatter.Number (formatNumber)
import Data.Function ((>>>))
import Data.Functor ((<#>))
import Data.HTTP.Method as HTTP.Method
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (class IsSymbol)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\), type (/\))
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
import Web.Event.EventTarget (EventListener)
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

      let payload = "%Main.purs%"
      delayedAction <- makeThrottledAction $ do
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
      graphConfiguration <- makeGraphConfiguration graph el

      searchBox <- createSearchBox graphConfiguration
      DOM.Node.appendChild (DOM.Element.toNode searchBox) (HTML.HTMLElement.toNode body)
      div <- createElement "div" "" Nothing
      DOM.Node.setTextContent "hello!" $ DOM.Element.toNode div
      DOM.Node.appendChild (DOM.Element.toNode div) (HTML.HTMLElement.toNode body)



      Event.EventTarget.addEventListener HTML.Event.EventTypes.click el false (HTML.HTMLElement.toEventTarget body)

    Nothing -> Console.log "no body"
  Console.log "END"


type NodeHash = String

data NodeState = Collapsed | Expanded

instance Show NodeState where
  show Collapsed = "Collapsed"
  show Expanded = "Expanded"

type GraphConfiguration =
  { show :: NodeHash -> Maybe NodeState -> Effect Unit
  , hide :: NodeHash -> Effect Unit
  , hidden :: NodeHash -> Effect Boolean
  }

makeGraphConfiguration :: Element -> EventListener -> Effect GraphConfiguration
makeGraphConfiguration graph onClickNode = do
  nodeStates <- Ref.new Object.empty
  renderGraph <- makeGraphRenderer graph nodeStates
  let show hash state = Ref.modify_ (Object.insert hash state) nodeStates *> updateGraph
      hide hash = Ref.modify_ (Object.delete hash) nodeStates *> updateGraph
      hidden hash = (hash `Object.member` _) <$> Ref.read nodeStates
      updateGraph = runAff $ renderGraph >>= \svg -> liftEffect $ do
        removeAllChildren $ DOM.Element.toNode graph
        decorateGraph svg onClickNode
        appendElement svg graph
  pure { show, hide, hidden }

makeGraphRenderer :: Element -> Ref (Object (Maybe NodeState)) -> Effect (Aff Element)
makeGraphRenderer graph nodeStates = makeThrottledAction $ do
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

decorateGraph :: Element -> EventListener -> Effect Unit
decorateGraph svg onClickNode = pure unit





type NodeSelection =
  { select :: Boolean -> NodeHash -> Effect Unit
  , selected :: NodeHash -> Effect Boolean
  , selection :: Effect (Array NodeHash)
  }

makeNodeSelection :: Effect NodeSelection
makeNodeSelection = undefined

createSearchBox :: GraphConfiguration -> Effect Element
createSearchBox graphConfiguration = example *> do
  overlay <- createElement "div" "SearchBoxOverlay" Nothing
  searchBox <- createElement "div" "SearchBox" $ Just overlay
  searchBar <- createElement "div" "SearchBar" $ Just searchBox
  patternInput <- createElement "input" "PatternInput" $ Just searchBar
  let title = "The search pattern is a matched against SkyValues using SQLite LIKE."
      placeholder = "Enter a search pattern here to find and display nodes "
                 <> "in the Skyframe graph (you may use % as a wildcard)"
  DOM.Element.setAttribute "placeholder" placeholder patternInput
  DOM.Element.setAttribute "title" title patternInput
  findNodes <- makeThrottledAction $ do
    pattern <- liftEffect $ map (fromMaybe "")
      $ DOM.Node.nodeValue $ DOM.Element.toNode patternInput
    result <- Affjax.post Affjax.ResponseFormat.json "/find"
      $ Just $ Affjax.RequestBody.json $ Argonaut.fromString
      $ "%" <> pattern <> "%"
    case result of
      Left error -> errorAff $ Affjax.printError error
      Right response -> pure $ response.body /\ pattern

  nodeCountMax <- Ref.new 0.0
  nodeCount <- createElement "span" "NodeCount" $ Just searchBar
  searchResults <- createElement "div" "SearchResults" $ Just searchBox
  let renderResults :: Element -> Number -> Object Json -> String -> Effect Unit
      renderResults searchResults total nodes pattern = do
        nodeCountMax <- Ref.modify (max total) nodeCountMax
        let content = fromRight "" $ formatNumber "0,0" total
        DOM.Node.setTextContent content $ DOM.Element.toNode nodeCount
        removeAllChildren $ DOM.Element.toNode searchResults
        sequence_ $ flip Object.toArrayWithKey nodes $ \hash node -> do
          undefined

  let updateSearch :: Effect Unit
      updateSearch = runAff $ findNodes >>= lmap Argonaut.toArray >>> case _ of
        Nothing /\ _ -> errorAff "findNodes: expected an array"
        Just arr /\ pattern -> case arr !! 0 /\ arr !! 1 of
          Just total /\ Just nodes -> do
            total <- case Argonaut.toNumber total of
              Nothing -> errorAff "findNodes: total is not a number"
              Just total -> pure total
            nodes <- case Argonaut.toObject nodes of
              Nothing -> errorAff "findNodes: nodes is not an object"
              Just nodes -> pure nodes
            liftEffect $ renderResults searchResults total nodes pattern
          _ -> errorAff "findNodes: expected a two element array"

  pure overlay

  where
    example = do
      graphConfiguration.show "0000cb183bd675f7bf107efecb68e6dce0b7d1c53ef203a1fab7b091cac28b2c" (Just Expanded)
      graphConfiguration.show "0000e57a2d2d6bf1c610940f7d44c0cfd9eb111431b0b22f2d0fedcc3e1c8506" (Just Expanded)
      graphConfiguration.show "00048fc362dc18865a71c4e1da5b6c317a59df020900a675c0a3eca37acfa9a4" (Just Expanded)
      graphConfiguration.show "0005b8f87a65924d59957321c1a76ea1ce18a775765540e2dada21fd873e9fd0" (Just Expanded)
      graphConfiguration.show "0005debdd94a42f93a00320448e476768e48de126dde83dd46532065a2f8bf85" (Just Expanded)



makeThrottledAction :: forall a. Aff a -> Effect (Aff a)
makeThrottledAction action = do
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

errorAff :: forall a. String -> Aff a
errorAff = Aff.throwError <<< Aff.error

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

removeAllChildren :: Node -> Effect Unit
removeAllChildren node =
  DOM.Node.firstChild node >>= case _ of
    Nothing -> pure unit
    Just child -> do
      DOM.Node.removeChild child node
      removeAllChildren node
