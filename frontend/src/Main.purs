module Main where
import Affjax.RequestBody as Affjax.RequestBody
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Affjax.Web as Affjax
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Argonaut
import Data.Array ((!!))
import Data.Array as Array
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Bifunctor (lmap)
import Data.Char as Char
import Data.Either (Either(..), fromRight)
import Data.Either as Either
import Data.Foldable (any, foldMap, foldl, for_, sequence_, traverse_)
import Data.Formatter.Number (formatNumber)
import Data.Function ((>>>))
import Data.Functor ((<#>))
import Data.HTTP.Method as HTTP.Method
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.Show as Show
import Data.String (replaceAll, split)
import Data.String (toLower, toUpper)
import Data.String.CodeUnits as String
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex
import Data.String.Regex.Unsafe as Regex
import Data.Symbol (class IsSymbol)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, sequence, traverse)
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Debug (trace)
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
import Effect.Timer as Timer
import Foreign.Object (Object)
import Foreign.Object as Object
import Prelude
import Record as Record
import Signal (Signal)
import Signal as Signal
import Signal.Channel as Signal
import Signal.Effect as Signal
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.DOMTokenList as DOMTokenList
import Web.DOM.Document as Document
import Web.DOM.Element (Element)
import Web.DOM.Element as Element
import Web.DOM.HTMLCollection as HTMLCollection
import Web.DOM.Node (Node)
import Web.DOM.Node as Node
import Web.DOM.NonElementParentNode as NonElementParentNode
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
import Web.UIEvent.MouseEvent (MouseEvent)
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
  Just bodyElement -> do
    nodeConfiguration <- makeNodeConfiguration
    let body = HTMLElement.toElement bodyElement
    graph <- createElement "div" "graph" $ Just body
    nodeClickHandler <- makeTools graph nodeConfiguration
    attachGraphRenderer graph nodeConfiguration nodeClickHandler
    searchBox /\ focusInput <- createSearchBox nodeConfiguration
    appendElement searchBox body
    focusInput

type NodeHash = String

data NodeState = Collapsed | Expanded

instance Show NodeState where
  show Collapsed = "Collapsed"
  show Expanded = "Expanded"

type NodeConfiguration =
  { onChange :: (Maybe NodeHash -> Effect Unit) -> Effect Unit
  , get :: NodeHash -> Effect (Maybe NodeState)
  , show :: NodeHash -> NodeState -> Effect Unit
  , hide :: NodeHash -> Effect Unit
  , visible :: Effect (Array NodeHash)
  , json :: Effect Json
  }

makeNodeConfiguration :: Effect NodeConfiguration
makeNodeConfiguration = do
  nodeStates <- Ref.new Object.empty
  channel <- Signal.channel Nothing
  let notify = Signal.send channel
      onChange action = Signal.runSignal $ action <$> Signal.subscribe channel
      get hash = Object.lookup hash <$> Ref.read nodeStates
      show hash state = Ref.modify_ (Object.insert hash state) nodeStates *> notify (Just hash)
      hide hash = Ref.modify_ (Object.delete hash) nodeStates *> notify Nothing
      visible = Object.keys <$> Ref.read nodeStates
      json =  Argonaut.fromObject <<< map jsonState <$> Ref.read nodeStates
      jsonState = Argonaut.fromString <<< Show.show
  pure { onChange, get, show, hide, visible, json }

data ToolMode
  = Explore
  | Crop
  | Path
  | Find

type NodeClickHandler a = Element -> MouseEvent -> Effect a

makeTools :: Element -> NodeConfiguration -> Effect (NodeClickHandler Boolean)
makeTools graph nodeConfiguration = do
  handlers <- sequence [ makeCrop ]
  pure \element event ->
    let tryTools handlers = case Array.uncons handlers of
          Just { head: handler, tail: handlers} -> do
             handled <- handler element event
             if handled then pure true
               else tryTools handlers
          Nothing -> pure false
     in tryTools handlers

  where
    makeCrop :: Effect (NodeClickHandler Boolean)
    makeCrop = do
      active <- Ref.new false
      selection <- Ref.new Object.empty
      let updateDOM = nodes >>= traverse_ \node -> do
            hash <- Element.id node
            active <- Ref.read active
            selection <- Ref.read selection
            for_ [ "Selected", "Unselected" ] (removeClass node)
            when active $ case Object.lookup hash selection of
              Just _ -> addClass node "Selected"
              Nothing -> addClass node "Unselected"
      onKeyDown "Shift" do
        Ref.write true active
        Ref.write Object.empty selection
        updateDOM
      onKeyUp "Shift" do
        Ref.write false active
        updateDOM
        selection <- Ref.read selection
        when (not $ Object.isEmpty selection) $
          nodeConfiguration.visible >>= traverse_
            \hash -> if hash `Object.member` selection
              then pure unit else nodeConfiguration.hide hash
      pure \node event -> Ref.read active >>= not >>> if _ then pure false else do
        hash <- Element.id node
        let toggle = case _ of
              Just _ -> Nothing
              Nothing -> Just Collapsed
        Ref.modify_ (Object.alter toggle hash) selection
        updateDOM
        pure true

    nodes :: Effect (Array Element)
    nodes = getElementsByClassName "node" graph

attachGraphRenderer :: Element -> NodeConfiguration -> NodeClickHandler Boolean -> Effect Unit
attachGraphRenderer graph nodeConfiguration onClickNode = do
  render <- makeRenderer nodeConfiguration
  nodeConfiguration.onChange \changedNodeHash ->
    runAff $ render >>= \svg -> liftEffect do
      decorateGraph svg \element event -> do
        handled <- onClickNode element event
        if handled then pure unit else do
          hash <- Element.id element
          if MouseEvent.ctrlKey event
            then nodeConfiguration.hide hash
            else containsClass element "Expanded" >>= if _
              then nodeConfiguration.show hash Collapsed
              else nodeConfiguration.show hash Expanded
      removeAllChildren graph
      appendElement svg graph
      for_ changedNodeHash $ getElementById >=> case _ of
        Nothing -> Console.error "can't find changed node"
        Just element -> do
           addClass element "Changed"
           void $ Timer.setTimeout animationDuration do
              removeClass element "Changed"
              scrollIntoView element

  where
    makeRenderer :: NodeConfiguration -> Effect (Aff Element)
    makeRenderer nodeConfiguration = makeThrottledAction do
      result <- Affjax.post Affjax.ResponseFormat.document "/render"
        <<< Just <<< Affjax.RequestBody.json =<< liftEffect nodeConfiguration.json
      liftEffect $ case result of
        Left err -> error $ Affjax.printError err
        Right response -> do
          svg <- HTMLCollection.item 0 =<< Document.getElementsByTagName "svg" response.body
          case svg of
            Nothing -> error "svg element not found"
            Just svg -> pure svg

    decorateGraph :: Element -> NodeClickHandler Unit -> Effect Unit
    decorateGraph svg onClickNode = do
      getElementsByClassName "edge" svg >>= traverse_ animateFadeIn
      getElementsByClassName "node" svg >>= traverse_ \node -> do
        listener <- EventTarget.eventListener \event ->
          for_ (MouseEvent.fromEvent event) (onClickNode node)
        EventTarget.addEventListener EventTypes.click listener false $ Element.toEventTarget node
        hash <- Element.id node
        deconstructNodeElement node >>= case _ of
          Just (VisibleNode visibleNode) -> do
            addClass visibleNode.background "Selectable"
            prettyNodeType <- formatNodeType <$> textContent visibleNode.nodeType
            addClass node prettyNodeType
            setTextContent prettyNodeType visibleNode.nodeType
            addClass visibleNode.nodeType "NodeType"
            nodeData <- textContent visibleNode.nodeLabel
            let label = prettyNodeLabel hash prettyNodeType nodeData
                maxChars = 40
                ellipsis = if String.length label > maxChars then "…" else ""
            setTextContent (ellipsis <> String.takeRight maxChars label) visibleNode.nodeLabel
            addClass visibleNode.nodeLabel "NodeLabel"
            pure unit
          _ -> pure unit
        animateNodeTranslation node

    animateNodeTranslation :: Element -> Effect Unit
    animateNodeTranslation newNode = do
       let attr :: String -> Element -> MaybeT Effect Number
           attr name elem = MaybeT $ (Number.fromString =<< _) <$> Element.getAttribute name elem
           centerOf :: Element -> MaybeT Effect (Number /\ Number)
           centerOf = deconstructNodeElement >>> liftEffect >=> case _ of
             Nothing -> MaybeT $ Nothing <$ Console.error "failed to deconstruct node element"
             Just (VisibleNode vn) -> Tuple <$> attr "x" vn.nodeType <*> attr "y" vn.nodeType
             Just (HiddenNode hn) -> Tuple <$> attr "cx" hn.background <*> attr "cy" hn.background
       hash <- Element.id newNode
       getElementById hash >>= case _ of
         Nothing -> animateFadeIn newNode
         Just oldNode -> runMaybeT (sub <$> centerOf oldNode <*> centerOf newNode) >>= case _ of
           Nothing -> Console.error "unable to determine translation delta"
           Just (dx /\ dy) -> do
             let from = show dx <> " " <> show dy
                 duration = show animationDuration <> "ms"
             animate <- createSvgElement "animateTransform" newNode
             Element.setAttribute "attributeName" "transform" animate
             Element.setAttribute "attributeType" "XML" animate
             Element.setAttribute "type" "translate" animate
             Element.setAttribute "repeatCount" "1" animate
             Element.setAttribute "fill" "freeze" animate
             Element.setAttribute "dur" duration animate
             Element.setAttribute "from" from animate
             Element.setAttribute "to" "0 0" animate

    animateFadeIn :: Element -> Effect Unit
    animateFadeIn element = do
      let duration = show animationDuration <> "ms"
      animate <- createSvgElement "animate" element
      Element.setAttribute "attributeName" "opacity" animate
      Element.setAttribute "repeatCount" "1" animate
      Element.setAttribute "fill" "freeze" animate
      Element.setAttribute "dur" duration animate
      Element.setAttribute "values" "0;1" animate

    animationDuration :: Int
    animationDuration = 200

data NodeElement
  = HiddenNode { background :: Element }
  | VisibleNode { nodeType :: Element, nodeLabel :: Element, background :: Element }

deconstructNodeElement :: Element -> Effect (Maybe NodeElement)
deconstructNodeElement node = do
  paths <- getElementsByTagName "path" node
  texts <- getElementsByTagName "text" node
  ellipses <- getElementsByTagName "ellipse" node
  case paths /\ texts /\ ellipses of
    [ background ] /\ [ nodeType, nodeLabel ] /\ _ ->
      pure $ Just $ VisibleNode { nodeType, nodeLabel, background }
    _ /\ _ /\ [ background ] -> pure $ Just $ HiddenNode { background }
    _ -> pure Nothing

prettyNodeLabel :: NodeHash -> String -> String -> String
prettyNodeLabel hash nodeType nodeData = case nodeType of
  t | any (_ == t)
    [ "DirectoryListing"
    , "DirectoryListingState"
    , "File"
    , "FileState"
    , "WorkspaceFile"
    ] -> matching ".*\\[([^\\]]*)\\]\\/\\[([^\\]]*)\\]" nodeData $ case _ of
      [ Just m1, Just m2 ] -> m1 <> "/" <> m2
      _ -> default
  t | any (_ == t)
    [ "ActionExecution"
    , "ConfiguredTarget"
    , "TargetCompletion"
    ] -> matching "label=(.+), config" nodeData $ case _ of
      [ Just m1 ] -> m1
      _ -> default
  t | any (_ == t)
    [ "Artifact"
    ] -> matching "\\[.*\\]([^\\[\\]]+)" nodeData $ case _ of
      [ Just m1 ] -> m1
      _ -> default
  t | any (_ == t)
    [ "BzlLoad"
    , "ClientEnvironmentVariable"
    , "ContainingPackageLookup"
    , "IgnoredPackagePrefixes"
    , "Package"
    , "PackageLookup"
    , "Precomputed"
    , "RepositoryDirectory"
    ] -> matching ":(.*)" nodeData $ case _ of
      [ Just m1 ] -> m1
      _ -> default
  t | any (_ == t)
    [ "Glob"
    ] -> matching "subdir=(.*) pattern=(.+) globberOperation" nodeData $ case _ of
      [ Just m1, Just m2 ] -> m1 <> (if String.length m1 > 0 then "/" else "") <> m2
      _ -> default
  t | any (_ == t)
    [ "SingleToolchainResolution"
    ] -> matching "toolchainTypeLabel=(.+), targetPlatformKey" nodeData $ case _ of
      [ Just m1 ] -> m1
      _ -> default
  _ -> default
  where
    matching r s f =
      let regex = Regex.unsafeRegex r Regex.noFlags
      in case Regex.match regex s of
          Just matches -> f $ Array.NonEmpty.tail matches
          Nothing -> default
    default = String.take 32 hash

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
  previousPattern <- Ref.new Nothing
  findNodes <- makeThrottledAction do
    pattern <- liftEffect $ map (fromMaybe "")
      $ traverse HTMLInputElement.value
      $ HTMLInputElement.fromElement patternInput
    previous <- liftEffect $ Ref.read previousPattern
    liftEffect $ Ref.write (Just pattern) previousPattern
    if Just pattern == previous then error "pattern unchanged" else do
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
                renderPatternMatch row pattern nodeData

      renderPatternMatch :: Element -> String -> String -> Effect Unit
      renderPatternMatch row pattern nodeData =
        let regex = split (Pattern "%") pattern
                  # foldl (\fullRegex piece ->
                      let p /\ r = Pattern "_" /\ Replacement "."
                          pieceRegex = replaceAll p r $ escapeRegex piece
                      in fullRegex <> "(" <> pieceRegex <> ")(.*)") "(.*)"
                  # flip Regex.unsafeRegex Regex.ignoreCase
            escapeRegex = Regex.replace (Regex.unsafeRegex
              "[.*+?^${}()|[\\]\\\\]" Regex.global) "\\$&"
        in case Regex.match regex nodeData <#> Array.NonEmpty.drop 1 of
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
                            (max 0 (String.length content - 16))  -- Always show the last 16 chars of the first group,
                            (2 * max 0 (String.length nodeData - 180))  -- but show more if there is space to do so.
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

      onPatternInputEvent :: EventType -> (Event -> Effect Unit) -> Effect Unit
      onPatternInputEvent eventType handler = do
        let target = HTMLElement.toEventTarget
                 <$> HTMLElement.fromElement patternInput
        listener <- EventTarget.eventListener handler
        for_ target $ EventTarget.addEventListener eventType listener false

  for_ [ EventTypes.change, EventTypes.focus, EventTypes.input ]
    (_ `onPatternInputEvent` const (runAff updateSearch))

  let collapseSearch :: Effect Unit
      collapseSearch = do
        removeAllChildren searchResults
        updateNodeCount =<< Ref.read nodeCountMax
        Node.setNodeValue "" $ Element.toNode patternInput
        traverse_ (HTMLInputElement.setValue "") (HTMLInputElement.fromElement patternInput)
        traverse_ HTMLElement.blur (HTMLElement.fromElement patternInput)
        Ref.write Nothing previousPattern

  onKeyUp "Escape" collapseSearch

  let focusPatternInput :: Effect Unit
      focusPatternInput = traverse_ HTMLElement.focus
                        $ HTMLElement.fromElement patternInput
  pure $ overlay /\ focusPatternInput

onKeyUp :: String -> Effect Unit -> Effect Unit
onKeyUp = onKeyEvent EventTypes.keyup

onKeyDown :: String -> Effect Unit -> Effect Unit
onKeyDown = onKeyEvent EventTypes.keydown

onKeyEvent :: EventType -> String -> Effect Unit -> Effect Unit
onKeyEvent eventType key action = onDocumentEvent eventType \event ->
  for_ (KeyboardEvent.fromEvent event) \event ->
    if KeyboardEvent.key event == key
      then action else pure unit

onDocumentEvent :: EventType -> (Event -> Effect Unit) -> Effect Unit
onDocumentEvent eventType handler = do
  target <- Window.toEventTarget <$> HTML.window
  listener <- EventTarget.eventListener handler
  EventTarget.addEventListener eventType listener false target

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
  pure do
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
  when (id /= "") (Element.setId id element)
  for_ parent $ appendElement element
  pure element

createSvgElement :: String -> Element -> Effect Element
createSvgElement name parent = do
  doc <- HTMLDocument.toDocument <$> (Window.document =<< HTML.window)
  element <- Document.createElementNS (Just "http://www.w3.org/2000/svg") name doc
  appendElement element parent
  pure element

appendElement :: Element -> Element -> Effect Unit
appendElement element parent = do
  Node.appendChild (Element.toNode element) (Element.toNode parent)

setTextContent :: String -> Element -> Effect Unit
setTextContent content element
  = Node.setTextContent content
  $ Element.toNode element

textContent :: Element -> Effect String
textContent = Node.textContent <<< Element.toNode

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

containsClass :: Element -> String -> Effect Boolean
containsClass element className = do
  classList <- Element.classList element
  DOMTokenList.contains classList className

removeAllChildren :: Element -> Effect Unit
removeAllChildren element =
  let node = Element.toNode element
  in Node.firstChild node >>= case _ of
      Nothing -> pure unit
      Just child -> do
        Node.removeChild child node
        removeAllChildren element

getElementsByClassName :: String -> Element -> Effect (Array Element)
getElementsByClassName name element = HTMLCollection.toArray =<<
  Element.getElementsByClassName name element

getElementsByTagName :: String -> Element -> Effect (Array Element)
getElementsByTagName name element = HTMLCollection.toArray =<<
  Element.getElementsByTagName name element

getElementById :: String -> Effect (Maybe Element)
getElementById id = NonElementParentNode.getElementById id =<<
  HTMLDocument.toNonElementParentNode <$> (Window.document =<< HTML.window)

foreign import scrollIntoView :: Element -> Effect Unit

undefined = unsafeCoerce unit
