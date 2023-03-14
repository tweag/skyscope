module Main where

import Affjax.RequestBody as Affjax.RequestBody
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web as Affjax
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Argonaut (jsonEmptyObject) as Argonaut
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core (fromArray, fromString, isNull, toArray, toString) as Argonaut
import Data.Argonaut.Decode (decodeJson) as Argonaut
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..)) as Argonaut
import Data.Argonaut.Encode (encodeJson) as Argonaut
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Array as Array
import Data.Array.NonEmpty as Array.NonEmpty
import Data.DateTime as DateTime
import Data.DateTime.Instant as Instant
import Data.Either (Either(..), fromRight)
import Data.Foldable (any, foldMap, foldl, for_, sequence_, traverse_)
import Data.Formatter.Number (formatNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number as Number
import Data.Show as Show
import Data.String (replaceAll, split, toLower, toUpper)
import Data.String.CodeUnits as String
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.String.Regex (match, replace, split) as Regex
import Data.String.Regex.Flags (global, ignoreCase, noFlags) as Regex
import Data.String.Regex.Unsafe (unsafeRegex) as Regex
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
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
import Effect.Now (now)
import Effect.Ref as Ref
import Effect.Timer as Timer
import Foreign.Object (Object)
import Foreign.Object as Object
import Prelude
import Signal (runSignal) as Signal
import Signal.Channel (Channel)
import Signal.Channel (channel, send, subscribe) as Signal
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.DOMTokenList as DOMTokenList
import Web.DOM.Document as Document
import Web.DOM.Element (Element)
import Web.DOM.Element as Element
import Web.DOM.HTMLCollection as HTMLCollection
import Web.DOM.Node as Node
import Web.DOM.NonElementParentNode as NonElementParentNode
import Web.Event.Event (Event, EventType)
import Web.Event.EventTarget as EventTarget
import Web.HTML as HTML
import Web.HTML.Event.EventTypes (change, click, focus, input, load) as EventTypes
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes (keydown, keyup) as EventTypes
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.MouseEvent.EventTypes (mouseenter, mouseleave, mousemove) as EventTypes

main :: Effect Unit
main = do
  Console.log "Skyscope 🔭"
  target <- Window.toEventTarget <$> HTML.window
  listener <-EventTarget.eventListener $ const load
  EventTarget.addEventListener EventTypes.load listener false target

load :: Effect Unit
load = HTML.window >>= Window.document >>= HTMLDocument.body >>= case _ of
  Nothing -> error "html <body> element not found"
  Just bodyElement -> do
    nodeConfiguration <- makeNodeConfiguration
    restore nodeConfiguration
    let body = HTMLElement.toElement bodyElement
    graph <- createElement "div" "Graph" $ Just body
    nodeClickHandler <- makeTools graph nodeConfiguration
    renderState <- attachGraphRenderer graph nodeConfiguration nodeClickHandler
    searchBox /\ focusInput <- createSearchBox nodeConfiguration
    appendElement searchBox body
    tray <- createTray nodeConfiguration renderState
    appendElement tray body
    focusInput

restore :: NodeConfiguration -> Effect Unit
restore nodeConfiguration = do
  checkpoint <- getCheckpoint
  when (not $ Argonaut.isNull checkpoint) do
    nodeConfiguration.set checkpoint

type NodeHash = String

data NodeState = Collapsed | Expanded

instance decodeJsonNodeState :: DecodeJson NodeState where
  decodeJson json = case Argonaut.toString json of
    Just "Collapsed" -> Right Collapsed
    Just "Expanded" -> Right Expanded
    _ -> Left $ Argonaut.UnexpectedValue json

instance encodeJsonNodeState :: EncodeJson NodeState where
  encodeJson = Argonaut.fromString <<< show

instance Show NodeState where
  show Collapsed = "Collapsed"
  show Expanded = "Expanded"

type NodeConfiguration =
  { onChange :: (Maybe NodeHash -> Effect Unit) -> Effect Unit
  , atomically :: Effect (Maybe NodeHash) -> Effect Unit
  , show :: NodeHash -> NodeState -> Effect Unit
  , hide :: NodeHash -> Effect Unit
  , visible :: Effect (Array NodeHash)
  , set :: Json -> Effect Unit
  , get :: Effect Json
  }

makeNodeConfiguration :: Effect NodeConfiguration
makeNodeConfiguration = do
  atomicSemaphore <- Ref.new 0
  nodeStates <- Ref.new Object.empty
  channel <- Signal.channel Nothing
  let notify = Signal.send channel
      modify changed f = atomically $ changed <$ Ref.modify_ f nodeStates
      onChange action = Signal.runSignal $ action <$> Signal.subscribe channel

      atomically action = do
        reentrancyCount <- Ref.modify (_ + 1) atomicSemaphore
        changed <- action
        historyState <- Tuple changed <$> Ref.read nodeStates
        when (reentrancyCount == 1) do
          pushHistory $ Argonaut.encodeJson historyState
        notify changed
        Ref.modify_ (_ - 1) atomicSemaphore

      show hash state = modify (Just hash) (Object.insert hash state)
      hide hash = modify (Just hash) (Object.delete hash)
      visible = Object.keys <$> Ref.read nodeStates

      set json = case Argonaut.decodeJson json of
        Right s -> Ref.write s nodeStates *> notify Nothing
        Left err -> error $ Show.show err
      get = Argonaut.encodeJson <$> Ref.read nodeStates

  onPopHistory $ Argonaut.decodeJson >>> case _ of
    Left err -> error $ "failed to decode history: " <> Show.show err
    Right (changed /\ state) -> Ref.write state nodeStates *> notify changed

  pure { onChange, show, hide, atomically, visible, set, get }

data Click
  = NodeClick Element
  | PathClick Element

type ClickHandler a = Click -> MouseEvent -> Effect a

makeTools :: Element -> NodeConfiguration -> Effect (ClickHandler Boolean)
makeTools graph nodeConfiguration = do
  clickHandlers <- sequence
    [ makeOpenAllPaths
    , makeOpenPath
    , makeCrop
    ]
  pure \element event ->
    let tryTools handlers = case Array.uncons handlers of
          Just { head: handler, tail: handlers'} -> do
             handled <- handler element event
             if handled then pure true
               else tryTools handlers'
          Nothing -> pure false
     in tryTools clickHandlers

  where
    makeOpenAllPaths :: Effect (ClickHandler Boolean)
    makeOpenAllPaths = do
      active <- Ref.new false
      let openAllPaths = traversePaths openPath
          updateDOM = traversePaths \element ->
            Ref.read active >>= if _
              then addClass element "Animate"
              else removeClass element "Animate"
          traversePaths f = pathElements >>= traverse f
      onKeyDown "Shift" $ Ref.write true active <* updateDOM
      onKeyUp "Shift" $ Ref.write false active <* updateDOM
      pure \click _ -> Ref.read active >>= not >>>
        if _ then pure false else case click of
          PathClick _ -> openAllPaths $> true
          _ -> pure false

    makeOpenPath :: Effect (ClickHandler Boolean)
    makeOpenPath = pure \click _ -> case click of
      PathClick edge -> openPath edge $> true
      _ -> pure false

    openPath :: Element -> Effect Unit
    openPath edge = Element.id edge <#> split (Pattern "_") >>= case _ of
      [ origin, destination ] -> launchAff do
          url <- liftEffect $ getImportId <#> (_ <> "/path")
          result <- Affjax.post Affjax.ResponseFormat.json url
            $ Just $ Affjax.RequestBody.json $ Argonaut.fromArray
            [ Argonaut.fromString origin
            , Argonaut.fromString destination
            ]
          case result of
            Right response -> case join $ Argonaut.toArray response.body <#> traverse Argonaut.toString of
              Just path -> liftEffect $ nodeConfiguration.atomically $ Nothing <$ do
                for_ (orderOutsideIn path) $ flip nodeConfiguration.show Collapsed
              Nothing -> error "unexpected path results json"
            Left err -> error $ Affjax.printError err
      _ -> error "malformed edge id"

    makeCrop :: Effect (ClickHandler Boolean)
    makeCrop = do
      activeRef <- Ref.new false
      selectionRef <- Ref.new Object.empty
      let updateDOM = nodeElements >>= traverse_ \node -> do
            hash <- Element.id node
            active <- Ref.read activeRef
            selection <- Ref.read selectionRef
            for_ [ "Selected", "Unselected" ] (removeClass node)
            when active $ case Object.lookup hash selection of
              Just _ -> addClass node "Selected"
              Nothing -> addClass node "Unselected"
          begin = do
            Ref.write true activeRef
            updateDOM
          clear = do
            Ref.write false activeRef
            Ref.write Object.empty selectionRef
            updateDOM
          commit = do
            selection <- Ref.read selectionRef
            when (not $ Object.isEmpty selection) $
              nodeConfiguration.atomically $ Nothing <$ do
                nodeConfiguration.visible >>= traverse_
                  \hash -> if hash `Object.member` selection
                    then pure unit else nodeConfiguration.hide hash
            clear
      onKeyDown "Shift" begin
      onKeyUp "Shift" commit
      onElementEvent graph EventTypes.mouseenter \event ->
        for_ (MouseEvent.fromEvent event) \mouseEvent -> do
          if MouseEvent.shiftKey mouseEvent then pure unit else clear
      pure \click _ -> Ref.read activeRef >>= not >>> if _ then pure false else
        case click of
          NodeClick node -> do
            hash <- Element.id node
            let toggle = case _ of
                  Just _ -> Nothing
                  Nothing -> Just Collapsed
            Ref.modify_ (Object.alter toggle hash) selectionRef
            updateDOM
            pure true
          _ -> pure false

    nodeElements :: Effect (Array Element)
    nodeElements = getElementsByClassName "node" graph

    pathElements :: Effect (Array Element)
    pathElements = getElementsByClassName "Path" graph

orderOutsideIn :: forall a. Array a -> Array a
orderOutsideIn a = if Array.length a `mod` 2 == 0
  then case Array.unsnoc a of
    Just { init, last } -> [ last ] <> orderOutsideIn init
    Nothing -> a
  else case Array.uncons a of
    Just { head, tail } -> [ head ] <> orderOutsideIn tail
    Nothing -> a

data RenderState
  = RenderInit
  | Rendering Int
  | RenderDone Milliseconds
  | RenderFail (Maybe StatusCode)

instance Show RenderState where
  show = case _ of
    RenderInit -> "RenderInit"
    Rendering visibleCount -> "Rendering " <> show visibleCount
    RenderDone elapsedTime -> "RenderDone (" <> show elapsedTime <> ")"
    RenderFail statusCode -> "RenderFail (" <> show statusCode <> ")"

attachGraphRenderer :: Element -> NodeConfiguration -> ClickHandler Boolean -> Effect (Channel RenderState)
attachGraphRenderer graph nodeConfiguration onClick = do
  render <- makeRenderer
  renderState <- Signal.channel RenderInit
  renderState <$ nodeConfiguration.onChange \changed -> do
    let notify st = Signal.send renderState st
    startTime <- Instant.toDateTime <$> now
    let runAff = Aff.runAff_ $ case _ of
          Left err -> do
            logError $ show err
            notify $ RenderFail Nothing
          Right (Left statusCode) -> do
            notify $ RenderFail $ Just statusCode
          Right (Right _) -> do
            endTime <- Instant.toDateTime <$> now
            notify $ RenderDone $ DateTime.diff endTime startTime

    runAff do
      liftEffect do
        visibleCount <- Array.length <$> nodeConfiguration.visible
        notify $ Rendering visibleCount
      render >>= liftEffect <<< case _ of
        Left statusCode -> pure $ Left statusCode
        Right svg -> Right unit <$ do
          decorateGraph svg \click event -> do
            handled <- onClick click event
            if handled then pure unit else case click of
              NodeClick element -> do
                addClass element "Highlight"
                hash <- Element.id element
                if MouseEvent.ctrlKey event
                  then nodeConfiguration.hide hash
                  else containsClass element "Collapsed" >>= if _
                    then nodeConfiguration.show hash Expanded
                    else nodeConfiguration.show hash Collapsed
              _ -> pure unit
          removeAllChildren graph
          appendElement svg graph
          for_ changed $ getElementById >=> traverse \element -> do
            addClass element "Highlight"
            void $ Timer.setTimeout (2 * animationDuration) do
              scrollIntoView element
              void $ Timer.setTimeout animationDuration $
                removeClass element "Highlight"

  where
    makeRenderer :: Effect (Aff (Either StatusCode Element))
    makeRenderer = makeThrottledAction do
      url <- liftEffect $ getImportId <#> (_ <> "/render")
      result <- Affjax.post Affjax.ResponseFormat.document url
        <<< Just <<< Affjax.RequestBody.json =<< liftEffect nodeConfiguration.get
      liftEffect $ case result of
        Left err -> error $ Affjax.printError err
        Right response -> case response.status of
          StatusCode 200 -> do
            svg <- HTMLCollection.item 0 =<< Document.getElementsByTagName "svg" response.body
            maybe (error "svg element not found") (pure <<< Right) svg
          status -> pure $ Left status

    decorateGraph :: Element -> ClickHandler Unit -> Effect Unit
    decorateGraph svg handleClick = do
      getElementsByClassName "node" svg >>= traverse_ \node -> do
        listener <- EventTarget.eventListener \event ->
          for_ (MouseEvent.fromEvent event) (handleClick $ NodeClick node)
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
      getElementsByClassName "edge" svg >>= traverse_ \edge -> do
        containsClass edge "Path" >>= not >>> if _ then pure unit else do
          listener <- EventTarget.eventListener \event ->
            for_ (MouseEvent.fromEvent event) (handleClick $ PathClick edge)
          EventTarget.addEventListener EventTypes.click listener false $ Element.toEventTarget edge
        animateFadeIn edge


    animateNodeTranslation :: Element -> Effect Unit
    animateNodeTranslation newNode = do
       let attr :: String -> Element -> MaybeT Effect Number
           attr name elem = MaybeT $ (Number.fromString =<< _) <$> Element.getAttribute name elem
           centerOf :: Element -> MaybeT Effect (Number /\ Number)
           centerOf = deconstructNodeElement >>> liftEffect >=> case _ of
             Just (VisibleNode vn) -> Tuple <$> attr "x" vn.nodeType <*> attr "y" vn.nodeType
             Just (HiddenNode hn) -> Tuple <$> attr "cx" hn.background <*> attr "cy" hn.background
             Nothing -> error "failed to deconstruct node element"
       hash <- Element.id newNode
       getElementById hash >>= case _ of
         Nothing -> animateFadeIn newNode
         Just oldNode -> runMaybeT (sub <$> centerOf oldNode <*> centerOf newNode) >>= case _ of
           Nothing -> error "unable to determine translation delta"
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
             addClass animate "Animation"

    animateFadeIn :: Element -> Effect Unit
    animateFadeIn element = do
      let duration = show animationDuration <> "ms"
      animate <- createSvgElement "animate" element
      Element.setAttribute "attributeName" "opacity" animate
      Element.setAttribute "repeatCount" "1" animate
      Element.setAttribute "fill" "freeze" animate
      Element.setAttribute "dur" duration animate
      Element.setAttribute "values" "0;1" animate
      addClass animate "Animation"

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

data EdgeElement
  = PathElement { label :: Element }

deconstructEdgeElement :: Element -> Effect (Maybe EdgeElement)
deconstructEdgeElement edge = getElementsByTagName "text" edge <#> case _ of
  [ label ] -> Just $ PathElement { label }
  _ -> Nothing


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
  filterNodes <- makeThrottledAction do
    pattern <- liftEffect $ map (fromMaybe "")
      $ traverse HTMLInputElement.value
      $ HTMLInputElement.fromElement patternInput
    previous <- liftEffect $ Ref.read previousPattern
    liftEffect $ Ref.write (Just pattern) previousPattern
    if Just pattern == previous then error "pattern unchanged" else do
      url <- liftEffect $ getImportId <#> (_ <> "/filter")
      result <- Affjax.post Affjax.ResponseFormat.json url
        $ Just $ Affjax.RequestBody.json $ Argonaut.fromString
        $ "%" <> pattern <> "%"
      case result of
        Left err -> error $ Affjax.printError err
        Right response -> pure $ response.body /\ pattern

  nodeCountMaxRef <- Ref.new 0.0
  nodeCount <- createElement "span" "NodeCount" $ Just searchBar
  let updateNodeCount :: String -> Number -> Effect Unit
      updateNodeCount label total = setTextContent (label <> " " <>
        fromRight "" (formatNumber "0,0" total) <> " nodes") nodeCount

  searchResults <- createElement "div" "SearchResults" $ Just searchBox
  let renderResults :: NodeResults -> String -> Effect Unit
      renderResults results pattern = do
        removeAllChildren searchResults
        sequence_ $ Object.toArrayWithKey (renderResultRow pattern) results.resultNodes

      renderResultRow :: String -> NodeHash -> { nodeData :: String, nodeType :: String } -> Effect Unit
      renderResultRow pattern hash node = do
        row <- createElement "div" "" $ Just searchResults
        let updateSelected = visible >>= if _
              then removeClass row "Selected"
              else addClass row "Selected"
            toggleSelected = visible >>= if _
              then nodeConfiguration.show hash Collapsed *> updateSelected
              else nodeConfiguration.hide hash *> updateSelected
            visible = Array.notElem hash <$> nodeConfiguration.visible
        listener <- EventTarget.eventListener $ const toggleSelected
        EventTarget.addEventListener EventTypes.click
          listener false $ Element.toEventTarget row
        let prettyNodeType = formatNodeType node.nodeType
        addClass row prettyNodeType
        addClass row "ResultRow"
        setTitle node.nodeData row
        updateSelected
        typeSpan <- createElement "span" "" $ Just row
        setTextContent prettyNodeType typeSpan
        addClass typeSpan "NodeType"
        renderPatternMatch row pattern node.nodeData

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
            Nothing -> error "nodeData did not match regex"
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

  mouseOverSearchBox <- Ref.new false
  resetSearchBoxFade <- Ref.new Nothing <#> \fadeTimerId -> do
    traverse_ Timer.clearTimeout =<< Ref.read fadeTimerId
    removeClass searchBox "Fade"
    nodeConfiguration.visible >>= case _ of
      [] -> Ref.write Nothing fadeTimerId
      _ -> do
        delay <- Ref.read mouseOverSearchBox <#> if _ then 3000 else 100
        timerId <- Timer.setTimeout delay $ addClass searchBox "Fade"
        Ref.write (Just timerId) fadeTimerId

  onElementEvent searchBox EventTypes.mouseenter $ const $ Ref.write true mouseOverSearchBox *> resetSearchBoxFade
  onElementEvent searchBox EventTypes.mouseleave $ const $ Ref.write false mouseOverSearchBox *> resetSearchBoxFade
  onElementEvent searchBox EventTypes.mousemove $ const resetSearchBoxFade
  onElementEvent searchBox EventTypes.click $ const resetSearchBoxFade
  onScroll searchResults $ resetSearchBoxFade

  let updateSearch :: Boolean -> Aff Unit
      updateSearch render = filterNodes >>= \(resultsJson /\ pattern) ->
        case Argonaut.decodeJson resultsJson of
          Left err -> error $ "unexpected find results json: " <> show err
          Right results -> liftEffect do
            let total = results.resultTotalNodes
            void $ Ref.modify (max total) nodeCountMaxRef
            updateNodeCount (if render then "found" else "") total
            when render $ renderResults results pattern
  launchAff $ updateSearch false

  let onPatternInputEvent = onElementEvent patternInput
  for_ [ EventTypes.change, EventTypes.focus, EventTypes.input ] $
    flip onPatternInputEvent $ const do
      addClass searchBox "Expanded"
      launchAff $ updateSearch true
      resetSearchBoxFade

  let collapseSearch :: Effect Unit
      collapseSearch = do
        removeAllChildren searchResults
        updateNodeCount "" =<< Ref.read nodeCountMaxRef
        Node.setNodeValue "" $ Element.toNode patternInput
        traverse_ (HTMLInputElement.setValue "") (HTMLInputElement.fromElement patternInput)
        traverse_ HTMLElement.blur (HTMLElement.fromElement patternInput)
        Ref.write Nothing previousPattern
        removeClass searchBox "Expanded"

  onKeyUp "Escape" collapseSearch

  onDocumentEvent EventTypes.click $ const do
    overSearchBox <- Ref.read mouseOverSearchBox
    if overSearchBox then pure unit else collapseSearch

  let focusPatternInput :: Effect Unit
      focusPatternInput = traverse_ HTMLElement.focus
                        $ HTMLElement.fromElement patternInput
  pure $ overlay /\ (resetSearchBoxFade *> focusPatternInput)

type NodeResults =
  { resultTotalNodes :: Number
  , resultNodes :: Object { nodeData :: String, nodeType :: String }
  }

onKeyUp :: String -> Effect Unit -> Effect Unit
onKeyUp = onKeyEvent EventTypes.keyup

onKeyDown :: String -> Effect Unit -> Effect Unit
onKeyDown = onKeyEvent EventTypes.keydown

onKeyEvent :: EventType -> String -> Effect Unit -> Effect Unit
onKeyEvent eventType key action = onDocumentEvent eventType \event ->
  for_ (KeyboardEvent.fromEvent event) \keyboardEvent ->
    if KeyboardEvent.key keyboardEvent == key
      then action else pure unit

onDocumentEvent :: EventType -> (Event -> Effect Unit) -> Effect Unit
onDocumentEvent eventType handler = do
  target <- Window.toEventTarget <$> HTML.window
  listener <- EventTarget.eventListener handler
  EventTarget.addEventListener eventType listener false target

onElementEvent :: Element -> EventType -> (Event -> Effect Unit) -> Effect Unit
onElementEvent element eventType handler = do
  let target = HTMLElement.toEventTarget
           <$> HTMLElement.fromElement element
  listener <- EventTarget.eventListener handler
  for_ target $ EventTarget.addEventListener eventType listener false

formatNodeType :: String -> String
formatNodeType nodeType
  = removeUnshareable
  $ foldMap camel
  $ Regex.split regex nodeType
  where
    removeUnshareable s = fromMaybe s $ String.stripSuffix (Pattern " (unshareable)") s
    camel = String.uncons >>> case _ of
      Just word -> toUpper (String.singleton word.head) <> toLower word.tail
      Nothing -> ""
    regex = Regex.unsafeRegex "[_]+" Regex.noFlags

createTray :: NodeConfiguration -> Channel RenderState -> Effect Element
createTray nodeConfiguration renderState = do
  tray <- createElement "div" "Tray" Nothing
  let clear = removeAllChildren tray

      replaceDiv className populate = clear *> do
        div <- createElement "div" "" $ Just tray
        void $ Timer.setTimeout 100 $ addClass div className
        populate div

      createIcon content tooltip parent = do
        icon <- createElement "span" "Icon" $ Just parent
        Element.setAttribute "title" tooltip icon
        setTextContent content icon
        pure icon

  checkpointCandidate <- Ref.new Argonaut.jsonEmptyObject
  renderingCount <- Ref.new 0
  let whenStarting action = do
        n <- Ref.modify (_ + 1) renderingCount
        when (n == 1) action
      whenQuiescent action = do
        n <- Ref.modify (_ - 1) renderingCount
        when (n == 0) action

  Signal.runSignal $ Signal.subscribe renderState <#> case _ of
    RenderInit -> pure unit

    Rendering visibleCount -> replaceDiv "Rendering" \div -> do
      status <- createElement "span" "Status" $ Just div
      setTextContent ("Rendering " <> show visibleCount <> " nodes") status
      icon <- createIcon "⏳" "Interrupt" div
      listener <- EventTarget.eventListener \event ->
        for_ (MouseEvent.fromEvent event) \mouseEvent ->
          when (MouseEvent.button mouseEvent == 0) do
            Location.reload =<< Window.location =<< HTML.window
      EventTarget.addEventListener EventTypes.click listener false $ Element.toEventTarget icon
      whenStarting $ flip Ref.write checkpointCandidate =<< nodeConfiguration.get

    RenderDone (Milliseconds elapsed) -> whenQuiescent $ replaceDiv "RenderDone" \div -> do
      status <- createElement "span" "Status" $ Just div
      setTextContent (fromRight "" $ formatNumber "0,0" elapsed <#> (_ <> "ms")) status
      saveLink <- createElement "a" "Save" $ Just div
      void $ createIcon "💾" "Save image" saveLink
      setCheckpoint =<< Ref.read checkpointCandidate
      updateSaveLink

    RenderFail statusCode -> whenQuiescent $ replaceDiv "RenderFail" \div -> do
      status <- createElement "span" "Status" $ Just div
      let content = case statusCode of
            Nothing -> "Request failed"
            Just (StatusCode code) -> "Rendering failed: " <> show code
      setTextContent content status


  pure tray

makeThrottledAction :: forall a. Aff a -> Effect (Aff a)
makeThrottledAction action = do
  mutex <- Effect.AVar.new unit
  tokenRef <- Ref.new 0
  pure do
    token <- liftEffect $ Ref.modify (_ + 1) tokenRef
    let run = liftEffect (Ref.read tokenRef) >>= \t -> if t == token
          then action else Aff.throwError $ Aff.error "action superseded"
    Aff.bracket (AVar.take mutex) (flip AVar.put mutex) (const run)

launchAff :: Aff Unit -> Effect Unit
launchAff = Aff.runAff_ $ case _ of
  Left err -> logError $ show err
  _ -> pure unit

logError :: String -> Effect Unit
logError err = when (shouldShow err) (Console.error err)
  where
    shouldShow s = not $ Array.any (contains s)
      [ "superseded", "pattern unchanged" ]
    contains s = flip String.contains (show s) <<< Pattern

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

foreign import getImportId :: Effect String

foreign import scrollIntoView :: Element -> Effect Unit

foreign import onScroll :: Element -> Effect Unit -> Effect Unit

foreign import pushHistory :: Json -> Effect Unit

foreign import onPopHistory :: (Json -> Effect Unit) -> Effect Unit

foreign import updateSaveLink :: Effect Unit

foreign import setCheckpoint :: Json -> Effect Unit

foreign import getCheckpoint :: Effect Json

undefined :: forall a. a
undefined = unsafeCoerce unit
