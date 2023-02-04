module Main where

import Affjax.RequestBody as Affjax.RequestBody
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Affjax.Web as Affjax
import Control.Monad.Error.Class (class MonadThrow, throwError)
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
import Effect.Exception (Error)
import Effect.Exception as Exception
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
import Web.HTML.Event.EventTypes as EventTypes
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes as EventTypes
import Web.UIEvent.MouseEvent as MouseEvent

main :: Effect Unit
main = do
  Console.log "Skyscope 🔭"
  target <- Window.toEventTarget <$> HTML.window
  listener <-EventTarget.eventListener $ const load
  EventTarget.addEventListener EventTypes.load listener false target

load :: Effect Unit
load = HTML.window >>= Window.document >>= HTMLDocument.body >>= case _ of
  Nothing -> Console.error "html <body> element not found"
  Just body -> do
    --tools <- makeTools
    graph /\ nodeConfiguration <- makeGraph undefined
    searchBox /\ focusInput <- createSearchBox nodeConfiguration
    let bodyElement = HTMLElement.toElement body
    appendElement searchBox bodyElement
    appendElement graph bodyElement
    focusInput

--makeTools :: Effect EventListener
--makeTools =
--  let onClickNode = pure unit
--  in onClickNode

--type NodeSelection =
--  { select :: Boolean -> NodeHash -> Effect Unit
--  , selected :: NodeHash -> Effect Boolean
--  , selection :: Effect (Array NodeHash)
--  }

--makeNodeSelection :: Effect NodeSelection
--makeNodeSelection = undefined

type NodeHash = String

data NodeState = Collapsed | Expanded

instance Show NodeState where
  show Collapsed = "Collapsed"
  show Expanded = "Expanded"

type NodeConfiguration =
  { get :: NodeHash -> Effect (Maybe NodeState)
  , show :: NodeHash -> NodeState -> Effect Unit
  , hide :: NodeHash -> Effect Unit
  }

makeGraph :: EventListener -> Effect (Element /\ NodeConfiguration)
makeGraph onClickNode = do
  nodeStates <- Ref.new Object.empty
  graph <- createElement "div" "graph" Nothing
  renderGraph <- makeGraphRenderer graph nodeStates
  let get hash = Object.lookup hash <$> Ref.read nodeStates
      show hash state = Ref.modify_ (Object.insert hash state) nodeStates *> updateGraph
      hide hash = Ref.modify_ (Object.delete hash) nodeStates *> updateGraph
      updateGraph = runAff $ renderGraph >>= \svg -> liftEffect $ do
        removeAllChildren graph
        decorateGraph svg onClickNode
        appendElement svg graph
  pure $ graph /\ { get, show, hide }

makeGraphRenderer :: Element -> Ref (Object NodeState) -> Effect (Aff Element)
makeGraphRenderer graph nodeStates = makeThrottledAction $ do
  nodeStates <- liftEffect $ Ref.read nodeStates
  result <- Affjax.post Affjax.ResponseFormat.document "/render_purescript"
    $ Just $ Affjax.RequestBody.json $ Argonaut.fromObject
    $ Argonaut.fromString <<< show <$> nodeStates
  liftEffect $ case result of
    Left err -> error $ Affjax.printError err
    Right response -> do
      svg <- HTMLCollection.item 0 =<< Document.getElementsByTagName "svg" response.body
      case svg of
        Nothing -> error "svg element not found"
        Just svg -> pure svg

decorateGraph :: Element -> EventListener -> Effect Unit
decorateGraph svg onClickNode = pure unit

createSearchBox :: NodeConfiguration -> Effect (Element /\ Effect Unit)
createSearchBox nodeConfiguration = do
  overlay <- createElement "div" "SearchBoxOverlay" Nothing
  searchBox <- createElement "div" "SearchBox" $ Just overlay
  searchBar <- createElement "div" "SearchBar" $ Just searchBox

  patternInput <- createElement "input" "PatternInput" $ Just searchBar
  let title = "The search pattern is a matched against SkyValues using SQLite LIKE."
      placeholder = "Enter a search pattern here to find and display nodes "
                 <> "in the Skyframe graph (you may use % as a wildcard)"
  Element.setAttribute "placeholder" placeholder patternInput
  Element.setAttribute "title" title patternInput
  previousPattern <- Ref.new " "
  findNodes <- makeThrottledAction $ do
    pattern <- liftEffect $ map (fromMaybe "")
      $ traverse HTMLInputElement.value
      $ HTMLInputElement.fromElement patternInput
    previous <- liftEffect $ Ref.read previousPattern
    liftEffect $ Ref.write pattern previousPattern
    if pattern == previous then error "pattern unchanged" else do
      result <- Affjax.post Affjax.ResponseFormat.json "/find"
        $ Just $ Affjax.RequestBody.json $ Argonaut.fromString
        $ "%" <> pattern <> "%"
      case result of
        Left err -> error $ Affjax.printError err
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
                let updateSelected = nodeConfiguration.get hash >>= case _ of
                      Nothing -> removeClass row "Selected"
                      Just _ -> addClass row "Selected"
                    toggleSelected = nodeConfiguration.get hash >>= case _ of
                      Nothing -> nodeConfiguration.show hash Expanded *> updateSelected
                      Just _ -> nodeConfiguration.hide hash *> updateSelected
                listener <- EventTarget.eventListener $ const toggleSelected
                EventTarget.addEventListener EventTypes.click
                  listener false $ Element.toEventTarget row
                let prettyNodeType = formatNodeType nodeType
                addClass row prettyNodeType
                addClass row "ResultRow"
                setTitle nodeData row
                updateSelected
                typeSpan <- createElement "span" "" $ Just row
                setTextContent prettyNodeType typeSpan
                addClass typeSpan "NodeType"
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
                        addClass span $ if groupIndex `mod` 2 == 0 then "Context" else "Highlight"
                        let content = fromMaybe "" group
                        if groupIndex > 0
                          then setTextContent content span
                          else
                            let startIndex = min
                                  (max 0 (String.length content - 16))
                                  (2 * max 0 (String.length nodeData - 180))
                                ellipsis = if startIndex == 0 then "" else "…"
                            in setTextContent (ellipsis <> String.drop startIndex content) span

  let updateSearch :: Aff Unit
      updateSearch = findNodes >>= \(resultsJson /\ pattern) ->
        let results = do
              array <- Argonaut.toArray resultsJson
              total <- Argonaut.toNumber =<< array !! 0
              nodes <- Argonaut.toObject =<< array !! 1
              pure $ total /\ nodes
        in case results of
              Nothing -> error "unexpected results json"
              Just (total /\ nodes) -> liftEffect $
                renderResults total nodes pattern

  let collapseSearch :: Effect Unit
      collapseSearch = do
        removeAllChildren searchResults
        updateNodeCount =<< Ref.read nodeCountMax
        Node.setNodeValue "" $ Element.toNode patternInput
        traverse_ (HTMLInputElement.setValue "") (HTMLInputElement.fromElement patternInput)
        traverse_ HTMLElement.blur (HTMLElement.fromElement patternInput)

  let onPatternInputEvent :: EventType -> (Event -> Effect Unit) -> Effect Unit
      onPatternInputEvent eventType handler = do
        let target = HTMLElement.toEventTarget
                 <$> HTMLElement.fromElement patternInput
        listener <- EventTarget.eventListener handler
        for_ target $ EventTarget.addEventListener eventType listener false
  for_ [ EventTypes.change, EventTypes.focus, EventTypes.input ]
    (_ `onPatternInputEvent` const (runAff updateSearch))

  let onDocumentEvent :: EventType -> (Event -> Effect Unit) -> Effect Unit
      onDocumentEvent eventType handler = do
        target <- HTMLDocument.toEventTarget <$> (Window.document =<< HTML.window)
        listener <- EventTarget.eventListener handler
        EventTarget.addEventListener eventType listener false target
  onDocumentEvent EventTypes.keyup \event ->
    for_ (KeyboardEvent.fromEvent event) \event ->
      if KeyboardEvent.key event == "Escape"
        then collapseSearch else pure unit

  --runAff $ updateSearch *> liftEffect collapseSearch

  let focusPatternInput :: Effect Unit
      focusPatternInput = traverse_ HTMLElement.focus
                        $ HTMLElement.fromElement patternInput
  pure $ overlay /\ focusPatternInput

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
          then action else Aff.throwError $ Aff.error "action superseded"
    Aff.bracket (AVar.take mutex) (flip AVar.put mutex) (const run)

runAff :: Aff Unit -> Effect Unit
runAff = Aff.runAff_ $ case _ of
  Left err | shouldShow err -> Console.errorShow err
  _ -> pure unit
  where
    contains err = flip String.contains (show err) <<< Pattern
    shouldShow err = not $ Array.any (contains err)
      [ "action superseded", "pattern unchanged" ]

error :: forall m a. MonadThrow Error m => String -> m a
error = throwError <<< Exception.error

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

setTitle :: String -> Element -> Effect Unit
setTitle title element = for_
  (HTMLElement.fromElement element)
  (HTMLElement.setTitle title)

addClass :: Element -> String -> Effect Unit
addClass element className = do
  classList <- Element.classList element
  DOMTokenList.add classList className

removeClass :: Element -> String -> Effect Unit
removeClass element className = do
  classList <- Element.classList element
  DOMTokenList.remove classList className

removeAllChildren :: Element -> Effect Unit
removeAllChildren element =
  let node = Element.toNode element
  in Node.firstChild node >>= case _ of
      Nothing -> pure unit
      Just child -> do
        Node.removeChild child node
        removeAllChildren element

undefined = unsafeCoerce unit
