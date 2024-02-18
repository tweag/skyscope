module Main where

import Affjax.RequestBody as Affjax.RequestBody
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web as Affjax
import Control.Alt ((<|>))
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.State.Trans (StateT, evalStateT, get, put)
import Data.Argonaut (jsonEmptyObject) as Argonaut
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core (fromArray, fromNumber, fromObject, fromString, isNull, toArray, toObject, toString) as Argonaut
import Data.Argonaut.Decode (decodeJson) as Argonaut
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..)) as Argonaut
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Argonaut.Encode (encodeJson) as Argonaut
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Array ((:))
import Data.Array as Array
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Bifunctor (bimap, lmap, rmap)
import Data.DateTime as DateTime
import Data.DateTime.Instant as Instant
import Data.Either (Either(..), fromRight)
import Data.Foldable (and, foldMap, for_, or, sequence_, traverse_)
import Data.Formatter.Number (formatNumber)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Number as Number
import Data.Show as Show
import Data.String (joinWith, replaceAll, split, toLower, toUpper)
import Data.String.CodeUnits as String
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.String.Regex (match, replace, search, split) as Regex
import Data.String.Regex.Flags (global, ignoreCase, noFlags) as Regex
import Data.String.Regex.Unsafe (unsafeRegex) as Regex
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
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
import Effect.Ref (Ref)
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
import Web.Event.Event (Event, EventType(..))
import Web.Event.Event as Event
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
import Web.UIEvent.MouseEvent.EventTypes (dblclick, mouseenter, mouseleave, mousemove) as EventTypes

main :: Effect Unit
main = do
  Console.log "Skyscope 🔭"
  onWindowEvent EventTypes.load $ const load

load :: Effect Unit
load = HTML.window >>= Window.document >>= HTMLDocument.body >>= case _ of
  Nothing -> error "html <body> element not found"
  Just bodyElement -> do
    nodeConfiguration <- makeNodeConfiguration
    restore nodeConfiguration
    let body = HTMLElement.toElement bodyElement
    graph <- createElement "div" "Graph" $ Just body
    initiateSearch <- Ref.new $ const $ pure unit -- Will be set by SearchBox later.
    popupUnderlay /\ popupOverlay /\ graphEventHandler <- makeTools graph nodeConfiguration initiateSearch
    renderState <- attachGraphRenderer graph nodeConfiguration graphEventHandler
    searchBox <- createSearchBox nodeConfiguration initiateSearch
    tray <- createTray nodeConfiguration renderState
    appendElement popupUnderlay body
    appendElement tray body
    appendElement searchBox body
    appendElement popupOverlay body
    join $ Ref.read initiateSearch <*> pure Nothing

restore :: NodeConfiguration -> Effect Unit
restore nodeConfiguration = do
  checkpoint <- getCheckpoint
  when (not $ Argonaut.isNull checkpoint) do
    nodeConfiguration.set checkpoint

type NodeHash = String

data NodeState = Collapsed | Expanded

instance Eq NodeState where
  eq Collapsed Collapsed = true
  eq Expanded Expanded = true
  eq _ _ = false

instance DecodeJson NodeState where
  decodeJson json = case Argonaut.toString json of
    Just "Collapsed" -> Right Collapsed
    Just "Expanded" -> Right Expanded
    _ -> Left $ Argonaut.UnexpectedValue json

instance EncodeJson NodeState where
  encodeJson = Argonaut.fromString <<< show

instance Show NodeState where
  show Collapsed = "Collapsed"
  show Expanded = "Expanded"

type NodeConfiguration =
  { onChange :: (Maybe NodeHash -> Effect Unit) -> Effect Unit
  , atomically :: Aff (Maybe NodeHash) -> Aff Unit
  , show :: NodeHash -> NodeState -> Effect Unit
  , hide :: NodeHash -> Effect Unit
  , preview :: Array NodeHash -> Effect Unit
  , visible :: Effect (Array NodeHash)
  , set :: Json -> Effect Unit
  , get :: Effect (Json /\ Maybe Json)
  }

makeNodeConfiguration :: Effect NodeConfiguration
makeNodeConfiguration = do
  atomicSemaphore <- Ref.new 0
  lastPreviewing <- Ref.new Nothing
  nodeStates <- Ref.new $ Object.empty /\ Nothing
  channel <- Signal.channel Nothing
  let notify = Signal.send channel
      modify changed f
        = launchAff $ atomically $ liftEffect
        $ changed <$ Ref.modify_ (lmap f) nodeStates

      onChange action = Signal.runSignal $ action <$> Signal.subscribe channel

      atomically action = Aff.bracket
        (liftEffect $ Ref.modify (_ + 1) atomicSemaphore)
        (const $ liftEffect $ Ref.modify_ (_ - 1) atomicSemaphore)
        \reentrancyCount -> do
          changed <- action
          liftEffect do
            historyState /\ previewing <- bimap (Tuple changed) isJust <$> Ref.read nodeStates
            when (reentrancyCount == 1 && not previewing) do
              pushHistory $ Argonaut.encodeJson historyState
            notify changed

      show hash state = modify (Just hash) (Object.insert hash state)

      hide hash = modify (Just hash) (Object.delete hash)

      visible = Ref.read nodeStates <#> case _ of
        baseConfig /\ Nothing -> Object.keys baseConfig
        _ /\ Just previewConfig -> Object.keys previewConfig

      set json = case Argonaut.decodeJson json of
        Right s -> Ref.write (s /\ Nothing) nodeStates *> notify Nothing
        Left err -> error $ Show.show err

      get = bimap Argonaut.encodeJson (map Argonaut.encodeJson) <$> Ref.read nodeStates

      preview hashes = do
        changed <- Ref.read lastPreviewing
        previewConfig <- snd <$> Ref.read nodeStates
        let changed' /\ previewConfig' = case Array.uncons hashes of
              Just { head } -> Just head /\ Just (Object.fromFoldable $ hashes <#> (_ /\ Collapsed) )
              Nothing -> changed /\ Nothing
        if previewConfig == previewConfig' then pure unit else do
          Ref.modify_ (rmap $ const previewConfig') nodeStates
          case Array.uncons hashes of
            Just { head } -> Ref.write (Just head) lastPreviewing
            Nothing -> pure unit
          notify changed'

  onPopHistory $ Argonaut.decodeJson >>> case _ of
    Left err -> error $ "failed to decode history: " <> Show.show err
    Right (changed /\ state) -> Ref.write (state /\ Nothing) nodeStates *> notify changed

  pure { onChange, show, hide, atomically, visible, set, get, preview }

type InitiateSearch = Maybe NodeHash -> Effect Unit

type GraphEventHandler = GraphEvent -> Effect Boolean

data GraphEvent = GraphEvent Element GraphEventType MouseEvent

data GraphEventType
  = NodeClick
  | NodeMouseLeave
  | NodeMouseMove
  | PathClick

makeTools :: Element -> NodeConfiguration -> Ref InitiateSearch -> Effect (Element /\ Element /\ GraphEventHandler)
makeTools graph nodeConfiguration initiateSearch = do
  popupUnderlay /\ popupOverlay /\ hoverPopup <- makeHoverPopup

  eventHandlers <- sequence
    [ makeOpenAllPaths
    , makeOpenPath
    , makeCrop
    , makeToggleVisibility
    , pure hoverPopup
    ]

  pure $ popupUnderlay /\ popupOverlay /\ \graphEvent ->
    let tryTools handlers = case Array.uncons handlers of
          Just { head: handler, tail: handlers'} -> do
             handled <- handler graphEvent
             if handled then pure true
               else tryTools handlers'
          Nothing -> pure false
     in tryTools eventHandlers

  where
    makeToggleVisibility :: Effect GraphEventHandler
    makeToggleVisibility = do
      clicking <- Ref.new Nothing
      pure \(GraphEvent node eventType mouseEvent) -> case eventType of
        NodeClick -> true <$ do
          addClass node "Highlight"
          hash <- Element.id node
          Ref.read clicking >>= case _ of
            Just (firstHash /\ timeoutId) -> do
              Timer.clearTimeout timeoutId
              Ref.write Nothing clicking
              when (hash == firstHash) do
                Event.stopPropagation $ MouseEvent.toEvent mouseEvent
                join $ Ref.read initiateSearch <*> pure (Just hash)
            Nothing -> do
              if MouseEvent.ctrlKey || MouseEvent.altKey $ mouseEvent
                then nodeConfiguration.hide hash
                else containsClass node "Collapsed" >>= if _
                  then nodeConfiguration.show hash Expanded
                  else containsClass node "Expanded" >>= if _
                    then do
                      timeoutId <- Timer.setTimeout 250 do
                        nodeConfiguration.show hash Collapsed
                        Ref.write Nothing clicking
                      Ref.write (Just $ hash /\ timeoutId) clicking
                    else nodeConfiguration.show hash Collapsed
        _ -> pure false


    makeOpenAllPaths :: Effect GraphEventHandler
    makeOpenAllPaths = do
      active <- Ref.new false
      let openAllPaths = nodeConfiguration.atomically $ Nothing <$ do
            liftEffect pathElements >>= traverse openPath
          updateDOM = pathElements >>= traverse \element ->
            Ref.read active >>= if _
              then addClass element "Animate"
              else removeClass element "Animate"
      onKeyDown "Shift" $ Ref.write true active <* updateDOM
      onKeyUp "Shift" $ Ref.write false active <* updateDOM
      pure \(GraphEvent _ eventType _) -> Ref.read active >>= not >>>
        if _ then pure false else case eventType of
          PathClick -> launchAff openAllPaths $> true
          _ -> pure false

    makeOpenPath :: Effect GraphEventHandler
    makeOpenPath = pure \(GraphEvent edge eventType _) -> case eventType of
      PathClick -> launchAff (openPath edge) $> true
      _ -> pure false

    openPath :: Element -> Aff Unit
    openPath edge = liftEffect (Element.id edge <#> split (Pattern "_")) >>= case _ of
      [ origin, destination ] -> do
          url <- liftEffect $ getImportId <#> (_ <> "/path")
          result <- Affjax.post Affjax.ResponseFormat.json url
            $ Just $ Affjax.RequestBody.json $ Argonaut.fromArray
            [ Argonaut.fromString origin
            , Argonaut.fromString destination
            ]
          case result of
            Right response -> case join $ Argonaut.toArray response.body <#> traverse Argonaut.toString of
              Just path -> nodeConfiguration.atomically $ liftEffect $ Nothing <$ do
                for_ (orderOutsideIn path) $ flip nodeConfiguration.show Collapsed
              Nothing -> error "unexpected path results json"
            Left err -> error $ Affjax.printError err
      _ -> error "malformed edge id"

    makeCrop :: Effect GraphEventHandler
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
              launchAff $ nodeConfiguration.atomically $ liftEffect $ Nothing <$ do
                nodeConfiguration.visible >>= traverse_
                  \hash -> if hash `Object.member` selection
                    then pure unit else nodeConfiguration.hide hash
            clear
      onKeyDown "Shift" begin
      onKeyUp "Shift" commit
      onElementEvent graph EventTypes.mouseenter \event ->
        for_ (MouseEvent.fromEvent event) \mouseEvent -> do
          if MouseEvent.shiftKey mouseEvent then pure unit else clear
      pure \(GraphEvent node eventType _) -> Ref.read activeRef >>= not >>> if _ then pure false else
        case eventType of
          NodeClick -> do
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

data DiffLine
  = Added String
  | Deleted String
  | Changed String (String /\ String)

instance DecodeJson DiffLine where
  decodeJson json = do
    o <- Argonaut.decodeJson json
    let lookupString k = case Object.lookup k o of
          Just v -> case Argonaut.toString v of
            Just s -> Right s
            Nothing -> unexpected
          Nothing -> unexpected
    lookupString "tag" >>= case _ of
      "Added" -> Added <$> lookupString "contents"
      "Deleted" -> Deleted <$> lookupString "contents"
      "Changed" -> case Object.lookup "contents" o of
        Just c -> uncurry Changed <$> Argonaut.decodeJson c
        Nothing -> unexpected
      _ -> unexpected
    where
      unexpected :: forall a. Either JsonDecodeError a
      unexpected = Left $ Argonaut.UnexpectedValue json

makeHoverPopup :: Effect (Element /\ Element /\ GraphEventHandler)
makeHoverPopup = do
  popupState <- Ref.new { div: Nothing }

  underlayDiv <- createElement "div" "PopupUnderlay" Nothing
  leftDiv <- createElement "div" "LeftPane" $ Just underlayDiv
  rightDiv <- createElement "div" "RightPane" $ Just underlayDiv
  overlayDiv <- createElement "div" "PopupOverlay" Nothing

  let diffPopups :: Effect Unit
      diffPopups = do
        let getPopupDiv paneDiv = do
              firstChild <- Node.firstChild (Element.toNode paneDiv)
              pure $ join $ map Element.fromNode firstChild
        pinnedLeft <- getPopupDiv leftDiv
        pinnedRight <- getPopupDiv rightDiv
        case pinnedLeft /\ pinnedRight of
          Just popupDivLeft /\ Just popupDivRight -> do
            let getContent popupDiv = firstOrEmpty =<< getElementsByTagName "p" popupDiv
                getTitle popupDiv = firstOrEmpty =<< getElementsByClassName "Title"  popupDiv
                getConfig popupDiv = firstOrEmpty =<< getElementsByClassName "Label" popupDiv
                firstOrEmpty = Array.uncons >>> case _ of
                  Just { head } -> textContent head
                  Nothing -> pure ""
            titleLeft <- getTitle popupDivLeft
            titleRight <- getTitle popupDivRight
            case titleLeft /\ titleRight of
              "BuildConfiguration" /\ "BuildConfiguration" -> do
                lhs <- getContent popupDivLeft
                rhs <- getContent popupDivRight
                withDiff lhs rhs \difflines -> do
                  configLeft <- getConfig popupDivLeft
                  configRight <- getConfig popupDivRight
                  let label = "bazel config "
                        <> String.take 7 configLeft <> " "
                        <> String.take 7 configRight
                  traverse_ (setTextContent difflines) =<< getElementsByTagName "p" popupDivLeft
                  traverse_ (setTextContent "diff") =<< getElementsByClassName "Title" popupDivLeft
                  traverse_ (setTextContent label) =<< getElementsByClassName "Label" popupDivLeft
                  addClass popupDivLeft "Diff"
                  removeElement popupDivRight
                  pure unit
              _ -> pure unit
          _ -> pure unit

      dismissDiff :: Effect Unit
      dismissDiff = traverse_ removeElement =<< getElementsByClassName "Diff" underlayDiv

      withDiff :: String -> String -> (String -> Effect Unit) -> Effect Unit
      withDiff lhs rhs action = launchAff $ do
        result <- Affjax.post Affjax.ResponseFormat.json "diff"
          $ Just $ Affjax.RequestBody.json $ Argonaut.fromArray $ Argonaut.fromString <$> [ lhs, rhs ]
        case result of
          Left err -> error $ Affjax.printError err
          Right response -> case Argonaut.decodeJson response.body of
            Left err -> error $ "failed to decode difflines: " <> Show.show err
            Right difflines -> liftEffect $ action difflines

      triggerGraphPopup :: Element -> MouseEvent -> Effect Unit
      triggerGraphPopup node mouseEvent = do
        getElementsByClassName "Popup" overlayDiv >>= traverse_ \popupDiv -> do
          pinned <- containsClass popupDiv "Pinned"
          if pinned then pure unit else removeElement popupDiv
        nodeType <- fromMaybe "" <$> Element.getAttribute "data:nodeType" node
        title <- fromMaybe nodeType <$> Element.getAttribute "data:title" node
        nodeData <- fromMaybe "" <$> Element.getAttribute "data:nodeData" node
        label <- fromMaybe "" <$> Element.getAttribute "data:label" node
        popupDiv <- createPopup mouseEvent title label nodeData
        addClass popupDiv nodeType
        showPopup popupDiv

      showPopup :: Element -> Effect Unit
      showPopup popupDiv = do
        flip Ref.modify_ popupState \s -> s { div = Just popupDiv }
        appendElement popupDiv overlayDiv
        onElementEvent popupDiv EventTypes.click Event.stopPropagation
        onElementEvent popupDiv EventTypes.dblclick Event.stopPropagation
        getElementById "SearchBox" >>= case _ of
          Just searchBox -> addClass searchBox "Hide"
          Nothing -> pure unit

      createPopup :: MouseEvent -> String -> String -> String -> Effect Element
      createPopup mouseEvent title label content = do
        window <- HTML.window
        windowWidth <- Window.innerWidth window
        windowHeight <- Window.innerHeight window
        let mouseX = MouseEvent.clientX mouseEvent
            mouseY = MouseEvent.clientY mouseEvent
            centreX = windowWidth / 2
            centreY = windowHeight / 2
            popupWidth = 400
            popupHeight = 200
            popupX /\ popupY = case (mouseX < centreX) /\ (mouseY < centreY) of
              true /\ true -> {- Upper-left quadrant -} mouseX /\ mouseY
              true /\ false -> {- Lower-left quadrant -} mouseX /\ (mouseY - popupHeight)
              false /\ true -> {- Upper-right quadrant -} (mouseX - popupWidth) /\ mouseY
              false /\ false -> {- Lower-right quadrant -} (mouseX - popupWidth) /\ (mouseY - popupHeight)
            property name value = name <> ": " <> show value <> "px"
            style = joinWith "; "
              [ property "left" popupX
              , property "top" popupY
              , property "width" popupWidth
              , property "height" popupHeight
              ]
        div <- createElement "div" "" Nothing
        Element.setAttribute "style" style div
        addClass div "Popup"
        headerDiv <- createElement "div" "" $ Just div
        addClass headerDiv "Header"
        titleSpan <- createElement "span" "" $ Just headerDiv
        addClass titleSpan "Title"
        setTextContent title titleSpan
        labelSpan <- createElement "span" "" $ Just headerDiv
        addClass labelSpan "Label"
        setTextContent label labelSpan
        enableAutoSelect EventTypes.click labelSpan
        spacerSpan <- createElement "span" "" $ Just headerDiv
        addClass spacerSpan "Spacer"
        iconSpan <- createElement "span" "" $ Just headerDiv
        addClass iconSpan "Icon"
        setTextContent "📍" iconSpan
        onElementEvent iconSpan EventTypes.click $ const $ expandPopup div
        contentDiv <- createElement "div" "" $ Just div
        addClass contentDiv "Content"
        let tooltip = "Double click to select all."
        Element.setAttribute "title" tooltip contentDiv
        enableAutoSelect EventTypes.dblclick contentDiv
        paragraph <- createElement "p" "" $ Just contentDiv
        setTextContent content paragraph
        pure div

      expandPopup :: Element -> Effect Unit
      expandPopup popupDiv = do
        dismissDiff
        window <- HTML.window
        windowWidth <- toNumber <$> Window.innerWidth window
        popupMidX <- Element.getBoundingClientRect popupDiv <#> \bbox -> bbox.left + bbox.width / 2.0
        leftEmpty <- isNothing <$> Node.firstChild (Element.toNode leftDiv)
        rightEmpty <- isNothing <$> Node.firstChild (Element.toNode rightDiv)
        let favourLeft = popupMidX < windowWidth / 2.0
        paneDiv <- case leftEmpty /\ rightEmpty of
            false /\ false | favourLeft -> removeAllChildren leftDiv $> leftDiv
            false /\ false | otherwise -> removeAllChildren rightDiv $> rightDiv
            true /\ true | favourLeft -> pure leftDiv
            true /\ true | otherwise -> pure rightDiv
            true /\ false -> pure leftDiv
            false /\ true -> pure rightDiv
        Element.removeAttribute "style" popupDiv
        appendElement popupDiv paneDiv
        addClass popupDiv "Pinned"
        iconSpan <- createElement "span" "" Nothing
        addClass iconSpan "Icon"
        setTextContent "📍" iconSpan
        traverse_ removeElement =<< getElementsByClassName "Icon" popupDiv
        traverse_ (appendElement iconSpan) =<< getElementsByClassName "Header" popupDiv
        onElementEvent iconSpan EventTypes.click $ const $ removeElement popupDiv
        diffPopups

      nearbyPopup :: Element -> MouseEvent -> Effect Boolean
      nearbyPopup popupDiv mouseEvent = do
        region <- Element.getBoundingClientRect popupDiv <#> \bbox ->
          { left: bbox.left - margin, right: bbox.right + margin
          , top: bbox.top - margin, bottom: bbox.bottom + margin }
        let mouseX = toNumber $ MouseEvent.clientX mouseEvent
            mouseY = toNumber $ MouseEvent.clientY mouseEvent
        pure $ and
          [ mouseX > region.left
          , mouseX < region.right
          , mouseY > region.top
          , mouseY < region.bottom
          ]
        where
          margin = 25.0

      dismissPopup :: Element -> Effect Unit
      dismissPopup popupDiv = do
        Ref.modify_ _ { div = Nothing } popupState
        getElementById "SearchBox" >>= case _ of
          Just searchBox -> removeClass searchBox "Hide"
          Nothing -> pure unit
        containsClass popupDiv "Pinned" >>=
          if _ then pure unit else do
            removeElement popupDiv

  onWindowEvent EventTypes.mousemove \event -> do
    Ref.read popupState <#> _.div >>= case _ of
      Just popupDiv -> case MouseEvent.fromEvent event of
        Just mouseEvent -> nearbyPopup popupDiv mouseEvent
          >>= if _ then pure unit else dismissPopup popupDiv
        Nothing -> pure unit
      Nothing -> pure unit

  let dismissEvents =
        [ EventTypes.click
        , EventTypes.dblclick
        , EventType "scroll"
        ]

  for_ dismissEvents $ flip onWindowEvent $ const do
      Ref.read popupState <#> _.div >>= case _ of
        Just popupDiv -> dismissPopup popupDiv
        Nothing -> pure unit

  hoverTimer <- Ref.new Nothing
  pure $ underlayDiv /\ overlayDiv /\ \(GraphEvent node eventType mouseEvent) -> do
    traverse_ Timer.clearTimeout =<< Ref.read hoverTimer
    case eventType of
      NodeMouseLeave -> do
         Ref.write Nothing hoverTimer
         pure true
      NodeMouseMove -> do
        timeoutId <- Timer.setTimeout 500 $ triggerGraphPopup node mouseEvent
        Ref.write (Just timeoutId) hoverTimer
        pure true
      _ -> pure false

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

attachGraphRenderer :: Element -> NodeConfiguration -> GraphEventHandler -> Effect (Channel RenderState)
attachGraphRenderer graph nodeConfiguration onEvent = do
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
      render >>= case _ of
        Left statusCode -> pure $ Left statusCode
        Right svg -> Right unit <$ do
          let updateDocument = do
                removeAllChildren graph
                appendElement svg graph
                for_ changed $ getElementById >=> traverse \element -> do
                  addClass element "Highlight"
                  void $ Timer.setTimeout (2 * animationDuration) do
                    scrollIntoView element
                    void $ Timer.setTimeout animationDuration $
                      removeClass element "Highlight"
          decorateGraph svg updateDocument onEvent

  where
    makeRenderer :: Effect (Aff (Either StatusCode Element))
    makeRenderer = makeThrottledAction do
      let pickConfig = case _ of
            baseConfig /\ Nothing -> baseConfig
            _ /\ Just (previewConfig) -> previewConfig
      url <- liftEffect $ getImportId <#> (_ <> "/render")
      result <- Affjax.post Affjax.ResponseFormat.document url
        <<< Just <<< Affjax.RequestBody.json <<< pickConfig
        =<< liftEffect nodeConfiguration.get
      liftEffect $ case result of
        Left err -> error $ Affjax.printError err
        Right response -> case response.status of
          StatusCode 200 -> do
            svg <- HTMLCollection.item 0 =<< Document.getElementsByTagName "svg" response.body
            maybe (error "svg element not found") (pure <<< Right) svg
          status -> pure $ Left status

    decorateGraph :: Element -> Effect Unit -> GraphEventHandler -> Aff Unit
    decorateGraph svg updateDocument handleEvent = do
      let addMouseListener element graphEventType eventType =
            onElementEvent element eventType \event ->
              for_ (MouseEvent.fromEvent event)
                (handleEvent <<< GraphEvent element graphEventType)

      liftEffect $ getElementsByClassName "edge" svg >>= traverse_ \edge -> do
        containsClass edge "Path" >>= if _
          then addMouseListener edge PathClick EventTypes.click
          else pure unit
        animateFadeIn edge

      contents <- liftEffect $ map Array.catMaybes $ getElementsByClassName "node" svg >>= traverse \node -> do
        traverse_ removeElement =<< getElementsByTagName "title" node
        addMouseListener node NodeMouseLeave EventTypes.mouseleave
        addMouseListener node NodeMouseMove EventTypes.mousemove
        addMouseListener node NodeClick EventTypes.click
        animateNodeTranslation node
        toNodeElement node >>= case _ of
          Just { anchor, background, text } -> do
            Element.getAttribute "xlink:title" anchor >>= case _ of
              Just nodeData -> do
                let label = join $ Regex.match labelRegex nodeData <#> Array.NonEmpty.head
                    labelRegex = Regex.unsafeRegex "(@[.-\\w]+)?//(/?[^/:,}\\]]+)*(:[^/,}\\]]+(/[^/,}\\]]+)*)?" Regex.noFlags
                    config = join $ join $ Regex.match configRegex nodeData <#> Array.NonEmpty.tail >>> Array.head
                    configRegex = Regex.unsafeRegex "BuildConfigurationKey\\[([0-9a-f]{64})\\]" Regex.noFlags
                Element.setAttribute "data:label" (fromMaybe "" $ label <|> config) node
                addClass background "Selectable"
                case text of
                  Just { title, detail } -> do
                    let setDetail content = do
                          let maxChars = 40
                              ellipsis = if String.length content > maxChars then "…" else ""
                          setTextContent (ellipsis <> String.takeRight maxChars content) detail
                        setTitle titleContent = do
                           setTextContent titleContent title
                           Element.setAttribute "data:title" titleContent node
                        setTooltip content = do
                           Element.setAttribute "data:nodeData" content node
                    setDetail =<< flip fromMaybe label <$> textContent detail
                    nodeType <- formatNodeType <$> textContent title
                    Element.setAttribute "data:nodeType" nodeType node
                    Element.setAttribute "xlink:title" "" anchor
                    setTitle nodeType
                    setTooltip nodeData
                    addClass node nodeType
                    addClass title "NodeTitle"
                    addClass detail "NodeDetail"
                    let contextKey = case nodeType of
                          "BuildConfiguration" -> config
                          "ConfiguredTarget" -> label
                          "ActionExecution" ->
                            let regex = Regex.unsafeRegex "(?<=actionIndex=)[0-9]+" Regex.noFlags
                            in case join $ Regex.match regex nodeData <#> Array.NonEmpty.head of
                                Just actionIndex -> label <#> (_ <> (" " <> actionIndex))
                                Nothing -> Nothing
                          _ -> Nothing
                    pure $ Just { node, nodeType, nodeData, label, contextKey, setDetail, setTitle, setTooltip }
                  Nothing -> pure Nothing
              Nothing -> pure Nothing
          _ -> pure Nothing

      liftEffect updateDocument

      let contextKeys = Array.mapMaybe _.contextKey contents
      withContext contextKeys \context -> do
        for_ contents \nodeContent -> liftEffect do
          let contextData = flip Object.lookup context =<< nodeContent.contextKey
          formattedContent <- formatNodeContent $ Object.fromFoldable
            [ "type" /\ nodeContent.nodeType
            , "data" /\ nodeContent.nodeData
            , "label" /\ fromMaybe "" nodeContent.label
            , "context" /\ fromMaybe "" contextData
            ]
          let traverseFormatted field f =
                for_ (Object.lookup field formattedContent) \content ->
                  if content == "" then pure unit else f content
          traverseFormatted "title" nodeContent.setTitle
          traverseFormatted "detail" nodeContent.setDetail
          traverseFormatted "tooltip" nodeContent.setTooltip


    withContext :: Array String -> (Object String -> Aff Unit) -> Aff Unit
    withContext labels action = do
      url <- liftEffect $ getImportId <#> (_ <> "/context")
      result <- Affjax.post Affjax.ResponseFormat.json url
        $ Just $ Affjax.RequestBody.json $ Argonaut.fromArray $ Argonaut.fromString <$> labels
      case result of
        Left err -> error $ Affjax.printError err
        Right response -> case join $ Argonaut.toObject response.body <#> traverse Argonaut.toString of
          Nothing -> error "unexpected context results json"
          Just context -> action context

    animateNodeTranslation :: Element -> Effect Unit
    animateNodeTranslation newNode = do
       let attr :: String -> Element -> MaybeT Effect Number
           attr name elem = MaybeT $ (Number.fromString =<< _) <$> Element.getAttribute name elem
           centerOf :: Element -> MaybeT Effect (Number /\ Number)
           centerOf = toNodeElement >>> liftEffect >=> case _ of
             Just { text: Just { title } } -> Tuple <$> attr "x" title <*> attr "y" title
             Just { text: Nothing, background } -> Tuple <$> attr "cx" background <*> attr "cy" background
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

type NodeElement =
  { anchor :: Element
  , background :: Element
  , text :: Maybe
    { title :: Element
    , detail :: Element
    }
  }

toNodeElement :: Element -> Effect (Maybe NodeElement)
toNodeElement node = do
  paths <- getElementsByTagName "path" node
  texts <- getElementsByTagName "text" node
  ellipses <- getElementsByTagName "ellipse" node
  anchors <- getElementsByTagName "a" node
  case paths /\ texts /\ ellipses /\ anchors of
    [ background ] /\ [ title, detail ] /\ _ /\ [ anchor ] ->
      pure $ Just { anchor, background, text: Just { title, detail } }
    _ /\ _ /\ [ background ] /\ [ anchor ] ->
      pure $ Just { anchor, background, text: Nothing }
    _ -> pure Nothing

data EdgeElement
  = PathElement { label :: Element }

deconstructEdgeElement :: Element -> Effect (Maybe EdgeElement)
deconstructEdgeElement edge = getElementsByTagName "text" edge <#> case _ of
  [ label ] -> Just $ PathElement { label }
  _ -> Nothing

type NodeFields =
  { nodeType :: String
  , nodeBody :: String
  , nodeTooltip :: String
  , nodeContext :: String
  }

createSearchBox :: NodeConfiguration -> Ref InitiateSearch -> Effect Element
createSearchBox nodeConfiguration initiateSearch = do
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
  searchOrigin <- Ref.new Nothing
  filterNodes <- makeThrottledAction do
    origin <- liftEffect $ Ref.read searchOrigin
    let limit = if isJust origin then 16.0 else 64.0
    pattern <- liftEffect $ map (fromMaybe "")
      $ traverse HTMLInputElement.value
      $ HTMLInputElement.fromElement patternInput
    previous <- liftEffect $ Ref.read previousPattern
    liftEffect $ Ref.write (Just pattern) previousPattern
    if Just pattern == previous then error "pattern unchanged" else do
      url <- liftEffect $ getImportId <#> (_ <> "/filter")
      result <- Affjax.post Affjax.ResponseFormat.json url
        $ Just $ Affjax.RequestBody.json $
          Argonaut.fromObject $ Object.fromFoldable
            [ "filterPattern" /\ Argonaut.fromString ("%" <> pattern <> "%")
            , "filterOrigin" /\ Argonaut.encodeJson origin
            , "filterLimit" /\ Argonaut.fromNumber limit
            ]
      case result of
        Left err -> error $ Affjax.printError err
        Right response -> pure $ response.body /\ pattern

  nodeCountMaxRef <- Ref.new 0.0
  nodeCount <- createElement "span" "NodeCount" $ Just searchBar
  let updateNodeCount :: String -> Number -> Effect Unit
      updateNodeCount label total = setTextContent (label <> " " <>
        fromRight "" (formatNumber "0,0" total) <> " nodes") nodeCount

  searchResults <- createElement "div" "SearchResults" $ Just searchBox

  let collapseSearch :: Effect Unit
      collapseSearch = do
        removeAllChildren searchResults
        updateNodeCount "" =<< Ref.read nodeCountMaxRef
        Node.setNodeValue "" $ Element.toNode patternInput
        traverse_ (HTMLInputElement.setValue "") (HTMLInputElement.fromElement patternInput)
        traverse_ HTMLElement.blur (HTMLElement.fromElement patternInput)
        Ref.write Nothing previousPattern
        removeClass searchBox "Expanded"
        Ref.write Nothing searchOrigin
        nodeConfiguration.preview []

  let renderResults :: NodeResults -> String -> Effect Unit
      renderResults results pattern = do
        removeAllChildren searchResults
        Ref.read searchOrigin >>= case _ of
          Nothing -> sequence_ $ Object.toArrayWithKey (renderResultRow pattern) results.resultNodes
          Just origin -> nodeConfiguration.preview $ origin : Array.filter (_ /= origin) (Object.keys results.resultNodes)

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
        onElementEvent row EventTypes.click $ const toggleSelected
        let formattedNodeType = formatNodeType node.nodeType
        addClass row formattedNodeType
        addClass row "ResultRow"
        for_ (HTMLElement.fromElement row) (HTMLElement.setTitle node.nodeData)
        updateSelected
        typeSpan <- createElement "span" "" $ Just row
        setTextContent formattedNodeType typeSpan
        addClass typeSpan "NodeTitle"
        renderPatternMatch row pattern $ replaceAll (Pattern "\n") (Replacement " ") node.nodeData

      renderPatternMatch :: Element -> String -> String -> Effect Unit
      renderPatternMatch row pattern nodeData = do
        let patternPieces = split (Pattern "%") pattern
        spanElements <- evalStateT (findMatches nodeData patternPieces) 0
        for_ spanElements $ flip appendElement row

      findMatches :: String -> Array String -> StateT Int Effect (Array Element)
      findMatches nodeData patternPieces = do
        let createSpan cls content = liftEffect $ do
              span <- createElement "span" "" Nothing
              setTextContent content span
              addClass span cls
              pure span
        previousEnd <- get
        let remaining = String.drop previousEnd nodeData
        case Array.uncons patternPieces of
          Nothing -> Array.singleton <$> createSpan "Context" remaining
          Just { head, tail } -> do
            let p /\ r = Pattern "_" /\ Replacement "."
                escapeRegex = Regex.replace (Regex.unsafeRegex
                  "[.*+?^${}()|[\\]\\\\]" Regex.global) "\\$&"
                pieceRegexStr = replaceAll p r $ escapeRegex head
                pieceRegex = Regex.unsafeRegex pieceRegexStr Regex.ignoreCase
                regexSearch = Regex.search pieceRegex remaining <#> (_ + previousEnd)
                regexMatch = Regex.match pieceRegex remaining
            case regexSearch /\ regexMatch  of
                Nothing /\ Nothing -> pure []
                Just begin /\ Just matches -> do
                  let contextLenMax = 32
                      contextLen = begin - previousEnd
                      takeHalfContext = String.take $ contextLenMax / 2 - 1
                      context = if contextLen < contextLenMax
                        then String.take contextLen remaining
                        else (if previousEnd == 0 then "" else takeHalfContext remaining) <> "…"
                          <> takeHalfContext (String.drop (contextLen - contextLenMax / 2 + 1) remaining)
                      highlight = fromMaybe "" $ Array.NonEmpty.head matches
                  contextSpan <- createSpan "Context" context
                  highlightSpan <- createSpan "Highlight" highlight
                  put $ begin + String.length highlight
                  ([ contextSpan, highlightSpan ] <> _)
                    <$> findMatches nodeData tail
                _ -> error "regex inconsistency"

  mouseOverSearchBox <- Ref.new false
  resetSearchBoxFade <- Ref.new Nothing <#> \fadeTimeoutId -> do
    traverse_ Timer.clearTimeout =<< Ref.read fadeTimeoutId
    removeClass searchBox "Fade"
    disableFade <- or <$> sequence
      [ isJust <$> Ref.read searchOrigin
      , Array.null <$> nodeConfiguration.visible
      ]
    if disableFade then Ref.write Nothing fadeTimeoutId else do
      delay <- Ref.read mouseOverSearchBox <#> if _ then 10000 else 100
      timeoutId <- Timer.setTimeout delay $ addClass searchBox "Fade"
      Ref.write (Just timeoutId) fadeTimeoutId

  onElementEvent searchBox EventTypes.mouseenter $ const $ Ref.write true mouseOverSearchBox *> resetSearchBoxFade
  onElementEvent searchBox EventTypes.mouseleave $ const $ Ref.write false mouseOverSearchBox *> resetSearchBoxFade
  onElementEvent searchBox EventTypes.mousemove $ const resetSearchBoxFade
  onElementEvent searchBox EventTypes.click $ const resetSearchBoxFade
  onElementEvent searchResults (EventType "scroll") $ const resetSearchBoxFade

  resultHashes <- Ref.new []

  let updateSearch :: Boolean -> Aff Unit
      updateSearch render = filterNodes >>= \(resultsJson /\ pattern) ->
        case Argonaut.decodeJson resultsJson of
          Left err -> error $ "unexpected find results json: " <> show err
          Right results -> liftEffect do
            let total = results.resultTotalNodes
            Ref.modify_ (max total) nodeCountMaxRef
            updateNodeCount (if render then "found" else "") total
            Ref.write (Object.keys $ results.resultNodes) resultHashes
            when render $ renderResults results pattern
  launchAff $ updateSearch false

  let showAll hashes = do
        window <- HTML.window
        let count = show (Array.length hashes)
            message = "Make all " <> count <> " matching nodes visible?"
        confirmed <- Window.confirm message window
        if not confirmed then pure unit else do
          launchAff $ nodeConfiguration.atomically $ liftEffect $ Nothing <$ do
            for_ hashes  $ flip nodeConfiguration.show Collapsed

  let onPatternInputEvent = onElementEvent patternInput
  for_ [ EventTypes.change, EventTypes.focus, EventTypes.input, EventTypes.keydown ] $
    flip onPatternInputEvent \event -> if Event.type_ event == EventTypes.keydown
      then for_ (KeyboardEvent.fromEvent event) \keyboardEvent ->
        case KeyboardEvent.key keyboardEvent of
          "Enter" -> collapseSearch *> (showAll =<< Ref.read resultHashes)
          "Escape" -> collapseSearch
          _ -> pure unit
      else do
        addClass searchBox "Expanded"
        launchAff $ updateSearch true
        resetSearchBoxFade

  onKeyUp "Escape" collapseSearch

  onDocumentEvent EventTypes.click $ const do
    overSearchBox <- Ref.read mouseOverSearchBox
    if overSearchBox then pure unit else collapseSearch

  let focusPatternInput :: Effect Unit
      focusPatternInput = traverse_ HTMLElement.focus
                        $ HTMLElement.fromElement patternInput

      launchSearchOriginClassUpdater :: Effect Unit
      launchSearchOriginClassUpdater = do
        Ref.read searchOrigin >>= case _ of
          Just hash -> getElementById hash >>= case _ of
            Just element -> do
              addClass element "SearchOrigin"
              void $ Timer.setTimeout 750 $ removeClass element "SearchOrigin"
            Nothing -> pure unit
          Nothing -> pure unit
        void $ Timer.setTimeout 1250 launchSearchOriginClassUpdater

  launchSearchOriginClassUpdater

  flip Ref.write initiateSearch \hash -> do
    Ref.write hash searchOrigin
    resetSearchBoxFade
    focusPatternInput

  pure overlay

type NodeResults =
  { resultTotalNodes :: Number
  , resultNodes :: Object { nodeData :: String, nodeType :: String }
  }

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
      onElementEvent icon EventTypes.click \event ->
        for_ (MouseEvent.fromEvent event) \mouseEvent ->
          when (MouseEvent.button mouseEvent == 0) do
            Location.reload =<< Window.location =<< HTML.window
      whenStarting $ flip Ref.write checkpointCandidate <<< fst =<< nodeConfiguration.get

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

onKeyUp :: String -> Effect Unit -> Effect Unit
onKeyUp = onKeyEvent EventTypes.keyup

onKeyDown :: String -> Effect Unit -> Effect Unit
onKeyDown = onKeyEvent EventTypes.keydown

onKeyEvent :: EventType -> String -> Effect Unit -> Effect Unit
onKeyEvent eventType key action = onDocumentEvent eventType \event ->
  for_ (KeyboardEvent.fromEvent event) \keyboardEvent ->
    if KeyboardEvent.key keyboardEvent == key
      then action else pure unit

onWindowEvent :: EventType -> (Event -> Effect Unit) -> Effect Unit
onWindowEvent eventType handler = do
  target <- Window.toEventTarget <$> HTML.window
  listener <-EventTarget.eventListener handler
  EventTarget.addEventListener eventType listener false target

onDocumentEvent :: EventType -> (Event -> Effect Unit) -> Effect Unit
onDocumentEvent eventType handler = do
  target <- Window.toEventTarget <$> HTML.window
  listener <- EventTarget.eventListener handler
  EventTarget.addEventListener eventType listener false target

onElementEvent :: Element -> EventType -> (Event -> Effect Unit) -> Effect Unit
onElementEvent element eventType handler = do
  let target = Element.toEventTarget element
  listener <- EventTarget.eventListener handler
  EventTarget.addEventListener eventType listener false target

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

removeElement :: Element -> Effect Unit
removeElement element = do
  let node = Element.toNode element
  parent <- Node.parentNode node
  for_ parent $ Node.removeChild node

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

foreign import enableAutoSelect :: EventType -> Element -> Effect Unit

foreign import scrollIntoView :: Element -> Effect Unit

foreign import pushHistory :: Json -> Effect Unit

foreign import onPopHistory :: (Json -> Effect Unit) -> Effect Unit

foreign import updateSaveLink :: Effect Unit

foreign import setCheckpoint :: Json -> Effect Unit

foreign import getCheckpoint :: Effect Json

foreign import formatNodeContent :: Object String -> Effect (Object String)

undefined :: forall a. a
undefined = unsafeCoerce unit
