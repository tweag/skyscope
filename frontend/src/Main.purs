module Main where

import Affjax.RequestBody as Affjax.RequestBody
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Affjax.Web as Affjax
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Argonaut
import Data.Array ((!!))
import Data.Array as Array
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Bifunctor (lmap)
import Data.Char as Char
import Data.Either (Either(..), fromRight)
import Data.Either as Either
import Data.Foldable (foldMap, foldl, for_, sequence_, traverse_)
import Data.Formatter.Number (formatNumber)
import Data.Function ((>>>))
import Data.Functor ((<#>))
import Data.HTTP.Method as HTTP.Method
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (replaceAll, split)
import Data.String (toLower, toUpper)
import Data.String.CodeUnits as String
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex
import Data.String.Regex.Unsafe as Regex
import Data.Symbol (class IsSymbol)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, traverse)
import Data.Tuple (uncurry)
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
import Prelude
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.DOMTokenList as DOMTokenList
import Web.DOM.Document as Document
import Web.DOM.Element (Element)
import Web.DOM.Element as Element
import Web.DOM.HTMLCollection as HTMLCollection
import Web.DOM.Node (Node)
import Web.DOM.Node as Node
import Web.Event.Event (Event, EventType, type_)
import Web.Event.EventTarget (EventListener)
import Web.Event.EventTarget as EventTarget
import Web.HTML as HTML
import Web.HTML.Event.EventTypes as Event.EventTypes
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes as KeyboardEvent.EventTypes

main :: Effect Unit
main = do
  Console.log "Skyscope 🔭"
  target <- Window.toEventTarget <$> HTML.window
  listener <-EventTarget.eventListener $ const load
  EventTarget.addEventListener Event.EventTypes.load listener false target
    
load :: Effect Unit
load = HTML.window >>= Window.document >>= HTMLDocument.body >>= case _ of
  Nothing -> Console.error "html <body> element not found"
  Just body -> do
    let bodyElement = HTMLElement.toElement body
    graph <- createElement "div" "graph" $ Just bodyElement
    graphConfiguration <- makeGraphConfiguration graph undefined
    searchBox <- createSearchBox graphConfiguration
    appendElement searchBox bodyElement
    appendElement graph bodyElement

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
        removeAllChildren graph
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
      svg <- HTMLCollection.item 0 =<< Document.getElementsByTagName "svg" response.body
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
  Element.setAttribute "placeholder" placeholder patternInput
  Element.setAttribute "title" title patternInput
  findNodes <- makeThrottledAction $ do
    pattern <- liftEffect $ map (fromMaybe "")
      $ traverse HTMLInputElement.value
      $ HTMLInputElement.fromElement patternInput
    result <- Affjax.post Affjax.ResponseFormat.json "/find"
      $ Just $ Affjax.RequestBody.json $ Argonaut.fromString
      $ "%" <> pattern <> "%"
    case result of
      Left error -> errorAff $ Affjax.printError error
      Right response -> pure $ response.body /\ pattern

  nodeCountMax <- Ref.new 0.0
  nodeCount <- createElement "span" "NodeCount" $ Just searchBar
  let updateNodeCount :: Number -> Effect Unit
      updateNodeCount total = setTextContent (fromRight ""
        (formatNumber "0,0" total) <> " nodes") nodeCount

  searchResults <- createElement "div" "SearchResults" $ Just searchBox
  let renderResults :: Number -> Object Json -> String -> Effect Unit
      renderResults total nodes pattern = do
        nodeCountMax <- Ref.modify (max total) nodeCountMax
        updateNodeCount total
        removeAllChildren searchResults
        sequence_ $ Object.toArrayWithKey (renderResultRow pattern) nodes
      renderResultRow :: String -> NodeHash -> Json -> Effect Unit
      renderResultRow pattern hash nodeJson =
        let node = do
              node <- Argonaut.toObject nodeJson
              nodeType <- Argonaut.toString =<< Object.lookup "nodeType" node
              nodeData <- Argonaut.toString =<< Object.lookup "nodeData" node
              pure $ nodeType /\ nodeData
        in case node of
              Nothing -> Console.error "unexpected node json"
              Just (nodeType /\ nodeData) -> do
                row <- createElement "div" "" $ Just searchResults
                addClass "ResultRow" row
                let prettyNodeType = formatNodeType nodeType
                addClass prettyNodeType row
                typeSpan <- createElement "span" "" $ Just row
                setTextContent prettyNodeType typeSpan
                addClass "NodeType" typeSpan
                
                let regex = split (Pattern "%") pattern
                          # foldl (\fullRegex piece ->
                              let p /\ r = Pattern "_" /\ Replacement "."
                                  pieceRegex = replaceAll p r $ escapeRegex piece
                              in fullRegex <> "(" <> pieceRegex <> ")(.*)") "(.*)"
                          # flip Regex.unsafeRegex Regex.ignoreCase
                    escapeRegex = Regex.replace (Regex.unsafeRegex
                      "[.*+?^${}()|[\\]\\\\]" Regex.global) "\\$&"
                case Regex.match regex nodeData <#> Array.NonEmpty.drop 1 of
                  Nothing -> Console.error "nodeData did not match regex"
                  Just matches ->
                    let matchCount = Array.length matches
                        indices = Array.range 0 matchCount
                    in for_ (indices `Array.zip` matches) \(groupIndex /\ group) -> do
                        span <- createElement "span" "" $ Just row
                        addClass (if groupIndex `mod` 2 == 0 then "Context" else "Highlight") span
                        let content = fromMaybe "" group
                        if groupIndex > 0
                          then setTextContent content span
                          else
                            let startIndex = min
                                  (max 0 (String.length content - 16))
                                  (2 * max 0 (String.length nodeData - 180))
                                ellipsis = if startIndex == 0 then "" else "…"
                            in setTextContent (ellipsis <> String.drop startIndex content) span

  let updateSearch :: Effect Unit
      updateSearch = runAff $ findNodes >>= \(resultsJson /\ pattern) ->
        let results = do
              array <- Argonaut.toArray resultsJson
              total <- Argonaut.toNumber =<< array !! 0
              nodes <- Argonaut.toObject =<< array !! 1
              pure $ total /\ nodes
        in case results of
              Nothing -> errorAff "unexpected results json"
              Just (total /\ nodes) -> liftEffect $
                renderResults total nodes pattern

  let collapseSearch :: Effect Unit
      collapseSearch = do
        removeAllChildren searchResults
        updateNodeCount =<< Ref.read nodeCountMax
        Node.setNodeValue "" $ Element.toNode patternInput
        traverse_ (HTMLInputElement.setValue "")
          (HTMLInputElement.fromElement patternInput)
        traverse_ HTMLElement.blur
          (HTMLElement.fromElement patternInput)


{-
    pattern <- liftEffect $ map (fromMaybe "")
      $ traverse HTMLInputElement.value
      $ HTMLInputElement.fromElement patternInput
-}


  let onPatternInputEvent :: EventType -> (Event -> Effect Unit) -> Effect Unit
      onPatternInputEvent eventType handler = do
        let target = HTMLElement.toEventTarget
                 <$> HTMLElement.fromElement patternInput
        listener <- EventTarget.eventListener handler
        for_ target $ EventTarget.addEventListener eventType listener false
  for_ [ Event.EventTypes.change
       , Event.EventTypes.focus
       , Event.EventTypes.input
       ] (_ `onPatternInputEvent` const updateSearch)

  let onDocumentEvent :: EventType -> (Event -> Effect Unit) -> Effect Unit
      onDocumentEvent eventType handler = do
        target <- HTMLDocument.toEventTarget
          <$> (Window.document =<< HTML.window)
        listener <- EventTarget.eventListener handler
        EventTarget.addEventListener eventType listener false target
  onDocumentEvent KeyboardEvent.EventTypes.keyup $ \event ->
    for_ (KeyboardEvent.fromEvent event) \event ->
      if KeyboardEvent.key event == "Escape"
        then collapseSearch else pure unit

  collapseSearch
  pure overlay

  where
    example = do
      graphConfiguration.show "001678e06f8e85e15249e54921ac2c3677f5f6ba783ea8cbf1f6dc51bbc117e7" (Just Expanded)
      --graphConfiguration.show "0000e57a2d2d6bf1c610940f7d44c0cfd9eb111431b0b22f2d0fedcc3e1c8506" (Just Expanded)
      --graphConfiguration.show "00048fc362dc18865a71c4e1da5b6c317a59df020900a675c0a3eca37acfa9a4" (Just Expanded)
      --graphConfiguration.show "0005b8f87a65924d59957321c1a76ea1ce18a775765540e2dada21fd873e9fd0" (Just Expanded)
      --graphConfiguration.show "0005debdd94a42f93a00320448e476768e48de126dde83dd46532065a2f8bf85" (Just Expanded)

formatNodeType :: String -> String
formatNodeType nodeType
  = removeUnshareable
  $ foldMap camel
  $ Regex.split regex nodeType
  where
    regex = Regex.unsafeRegex "[_]+" Regex.noFlags
    camel = String.uncons >>> case _ of
      Just word -> toUpper (String.singleton word.head) <> toLower word.tail
      Nothing -> ""
    removeUnshareable s = case String.stripSuffix (Pattern " (unshareable)") s of
      Just s -> s
      Nothing -> s
   
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
  doc <- Window.document =<< HTML.window
  element <- Document.createElement name $ HTMLDocument.toDocument doc
  Element.setId id element
  for_ parent $ appendElement element
  pure element

appendElement :: Element -> Element -> Effect Unit
appendElement element parent = do
  Node.appendChild (Element.toNode element) (Element.toNode parent)

setTextContent :: String -> Element -> Effect Unit
setTextContent content element
  = Node.setTextContent content
  $ Element.toNode element

addClass :: String -> Element -> Effect Unit
addClass className element = do
  classList <- Element.classList element
  DOMTokenList.add classList className

removeAllChildren :: Element -> Effect Unit
removeAllChildren element =
  let node = Element.toNode element
  in Node.firstChild node >>= case _ of
      Nothing -> pure unit
      Just child -> do
        Node.removeChild child node
        removeAllChildren element

undefined = unsafeCoerce unit
