{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | TODO
-- - [ ] allow changing command and path in stored session
-- - [ ] docs
-- - [ ] confirmation when session got stored (maybe just change the focus to it)
-- - [ ] split module so that some of the tmux stuff can be used as a library?
-- - [ ] pane layout editor (long-term)
module Tmux.Tui
  ( main,
  )
where

import qualified Brick
import Brick ((<+>), (<=>))
import qualified Brick.BChan as BChan
import qualified Brick.Focus as Focus
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Border.Style as Border
import qualified Brick.Widgets.Center as Center
import qualified Brick.Widgets.Dialog as Dialog
import qualified Brick.Widgets.Dialog as Dialog
import qualified Brick.Widgets.Edit as Edit
import qualified Cmd
import qualified Control.Exception.Safe as E
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as P
import Data.Char (isNumber)
import Data.Function ((&))
import Data.List (init)
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.Zipper as TZ
import qualified Data.Time as Time
import qualified Data.Time.Format as Format
import GHC.Base (divInt)
import GHC.Generics (Generic)
import qualified Graphics.Vty as Vty
import qualified Lens.Micro as L
import Lens.Micro ((^.))
import qualified Lens.Micro.TH as L
import qualified Petname
import Protolude
import qualified System.Directory as Dir
import System.Environment (getEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import qualified Text.Fuzzy as Fuzzy
import Tmux (IsActive (..), Pane (..), PaneId (..), Session (..), SessionId (..), Window (..), WindowId (..))
import qualified Tmux
import qualified Tmux.Stored
import Prelude (String)

data Model
  = Model
      { sessions :: [Session],
        windows :: [Window],
        panes :: [Pane],
        storedSessions :: [Tmux.Stored.Session],
        focus :: FocusRing,
        mode :: Mode,
        editor :: Edit.Editor Text Name,
        currentTime :: Time.UTCTime,
        searchEditor :: Edit.Editor Text Name,
        filterStored :: Maybe Text,
        filterSession :: Maybe Text,
        filterWindow :: Maybe Text,
        filterPane :: Maybe Text,
        moving :: Maybe MovingPaneOrWindow
      }

data Mode
  = Normal
  | RequireInput InputData
  | Confirm ConfirmData
  | Search SearchTarget

data MovingPaneOrWindow
  = MovingPane Pane
  | MovingWindow Window

data SearchTarget = SearchStored | SearchPane | SearchWindow | SearchSession
  deriving (Eq)

data InputData
  = InputData
      { inputCallback :: Callback,
        inputDialog :: Dialog.Dialog YesNo
      }

data ConfirmData
  = ConfirmData
      { message :: Text,
        action :: Callback,
        dialog :: Dialog.Dialog YesNo
      }

data Callback
  = Callback_ (Model -> IO ())
  | Callback (Model -> IO Model)

data Msg = Msg

data Name
  = FocusSession SessionId
  | FocusWindow WindowId
  | FocusPane PaneId
  | FocusNoSession
  | StoredSessionTitle Text
  | Panes
  | EditorLayer
  | SearchEditor
  | Cursor
  deriving (Ord, Eq, Show)

type FocusRing = Focus.FocusRing Name

data YesNo = Yes | No

L.makeLensesFor [("editor", "editorLens"), ("searchEditor", "searchEditorLens")] ''Model

main :: IO ()
main = do
  (_, exitCode) <- Cmd.run "tmux" ["server-info"]
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> do
      Cmd.run_ "tmux" ["new-session", "-d", "-s", "__TMUX_TUI_STARTUP"]
      Tmux.runCommand "__TMUX_TUI_STARTUP" $ Tmux.Command "tmux-tui"
      Cmd.run_ "tmux" ["attach", "-t", "__TMUX_TUI_STARTUP"]
      exitSuccess
  sessions <- Tmux.listSessions
  storedSessions <- Tmux.Stored.list
  let focus' =
        Focus.focusRing $
          (sessionToName <$> sessions)
            ++ (storedSessionToName <$> storedSessions)
            ++ ( case sessions of
                   [] -> [FocusNoSession]
                   _ -> []
               )
  let focus = case find ((== Active) . isAttached) sessions of
        Nothing -> focus'
        Just Session {sessionId} -> Focus.focusSetCurrent (FocusSession sessionId) focus'
  let editor = Edit.editorText EditorLayer (Just 1) ""
  let searchEditor = Edit.editorText SearchEditor (Just 1) ""
  currentTime <- Time.getCurrentTime
  let initialState =
        Model
          { windows = [],
            panes = [],
            sessions,
            focus,
            mode = Normal,
            editor,
            searchEditor,
            storedSessions,
            currentTime,
            filterStored = Nothing,
            filterSession = Nothing,
            filterWindow = Nothing,
            filterPane = Nothing,
            moving = Nothing
          }
  let buildVty = do
        v <- Vty.mkVty =<< Vty.standardIOConfig
        Vty.setMode (Vty.outputIface v) Vty.Mouse True
        return v
  initialVty <- buildVty
  Brick.customMain initialVty buildVty Nothing app =<< reloadRefocus initialState
  pure ()

app :: Brick.App Model Msg Name
app =
  Brick.App
    { Brick.appDraw = view,
      Brick.appChooseCursor = appCursor,
      Brick.appHandleEvent = handleEvent,
      Brick.appStartEvent = return,
      Brick.appAttrMap = const theMap
    }

appCursor :: Model -> [Brick.CursorLocation Name] -> Maybe (Brick.CursorLocation Name)
appCursor = Focus.focusRingCursor focus

handleEvent :: Model -> Brick.BrickEvent Name Msg -> Brick.EventM Name (Brick.Next Model)
handleEvent m@Model {focus} (Brick.MouseUp x _ _) = Brick.continue m {focus = Focus.focusSetCurrent x focus}
handleEvent m (Brick.VtyEvent ev) =
  case mode m of
    Normal -> case ev of
      Vty.EvKey Vty.KEsc [] -> handleEsc m
      Vty.EvKey Vty.KUp [] -> handleMoveUpDown Focus.focusPrev m & refresh
      Vty.EvKey Vty.KDown [] -> handleMoveUpDown Focus.focusNext m & refresh
      Vty.EvKey Vty.KLeft [] -> handleMoveLeft m & refresh
      Vty.EvKey Vty.KRight [] -> handleMoveRight m & refresh
      Vty.EvKey (Vty.KChar char) mods -> handleCharPress m char mods
      _ -> Brick.continue m
    RequireInput i@InputData {inputDialog, inputCallback} ->
      case ev of
        Vty.EvKey Vty.KEsc [] -> Brick.continue m {mode = Normal}
        Vty.EvKey Vty.KEnter [] ->
          case Dialog.dialogSelection inputDialog of
            Just Yes -> case inputCallback of
              Callback f -> liftIO (f m {mode = Normal}) >>= refresh
              Callback_ f -> liftIO (f m) >> refresh m {mode = Normal}
            Just No -> Brick.continue m {mode = Normal}
            Nothing -> Brick.continue m {mode = Normal}
        _ -> do
          newModel <- Brick.handleEventLensed m editorLens Edit.handleEditorEvent ev
          newDialog <- Dialog.handleDialogEvent ev inputDialog
          Brick.continue newModel {mode = RequireInput i {inputDialog = newDialog}}
    Confirm c@ConfirmData {action, dialog} ->
      case ev of
        Vty.EvKey Vty.KEsc [] -> Brick.continue m {mode = Normal}
        Vty.EvKey key [] | key == Vty.KChar 'q' -> Brick.continue m {mode = Normal}
        Vty.EvKey key [] | key == Vty.KEnter || key == Vty.KChar ' ' ->
          case Dialog.dialogSelection dialog of
            Just Yes -> liftIO (onConfirm action m) >>= refresh
            Just No -> Brick.continue m {mode = Normal}
            Nothing -> Brick.continue m {mode = Normal}
        _ -> do
          newDialog <- Dialog.handleDialogEvent ev dialog
          Brick.continue m {mode = Confirm c {dialog = newDialog}}
    Search target -> do
      nextModel <- case ev of
        Vty.EvKey Vty.KEsc [] ->
          let newM = case target of
                SearchStored -> m {filterStored = Nothing}
                SearchSession -> m {filterSession = Nothing}
                SearchWindow -> m {filterWindow = Nothing}
                SearchPane -> m {filterPane = Nothing}
           in pure newM {mode = Normal}
        Vty.EvKey Vty.KEnter [] ->
          pure $ updateFilter target m {mode = Normal}
        _ ->
          updateFilter target <$> Brick.handleEventLensed m searchEditorLens Edit.handleEditorEvent ev
      reloadedModel@Model {..} <- reloadRefocus nextModel
      Brick.continue
        reloadedModel
          { focus = case target of
              SearchStored -> case storedSessions of
                [] -> focus
                first : _ -> Focus.focusSetCurrent (StoredSessionTitle . Tmux.sessionTitle $ Tmux.Stored.session first) focus
              SearchSession -> case sessions of
                [] -> focus
                first : _ -> Focus.focusSetCurrent (FocusSession $ Tmux.sessionId first) focus
              SearchWindow -> case windows of
                [] -> focus
                first : _ -> Focus.focusSetCurrent (FocusWindow $ Tmux.windowId first) focus
              SearchPane -> case panes of
                [] -> focus
                first : _ -> Focus.focusSetCurrent (FocusPane $ Tmux.paneId first) focus
          }
handleEvent m _ = Brick.continue m

handleEsc :: Model -> Brick.EventM Name (Brick.Next Model)
handleEsc m@Model {..} =
  case (filterStored, filterSession, filterWindow, filterPane, Focus.focusGetCurrent focus) of
    (Just _, _, _, _, Just (StoredSessionTitle _)) -> refresh m {filterStored = Nothing}
    (_, Just _, _, _, Just (FocusSession _)) -> refresh m {filterSession = Nothing}
    (_, _, Just _, _, Just (FocusWindow _)) -> refresh m {filterWindow = Nothing}
    (_, _, _, Just _, Just (FocusPane _)) -> refresh m {filterPane = Nothing}
    _ ->
      case (moving, Focus.focusGetCurrent focus) of
        (Just _, Just (FocusPane _)) -> Brick.continue m {moving = Nothing}
        (Just _, Just (FocusWindow _)) -> Brick.continue m {moving = Nothing}
        _ -> Brick.halt m

updateFilter :: SearchTarget -> Model -> Model
updateFilter target m@Model {storedSessions, searchEditor} =
  let filter' = mconcat $ Edit.getEditContents searchEditor
      filter = if T.strip filter' == "" then Nothing else Just filter'
   in case target of
        SearchStored -> m {filterStored = filter}
        SearchSession -> m {filterSession = filter}
        SearchWindow -> m {filterWindow = filter}
        SearchPane -> m {filterPane = filter}

runRename :: MonadIO m => Model -> m ()
runRename = traverse_ (uncurry Tmux.rename) . focusedTmuxType

runCreate :: MonadIO m => Model -> m ()
runCreate m@Model {..} =
  case focusedTmuxType m of
    Just (title, type') ->
      Tmux.create title (Just type')
    Nothing ->
      case Focus.focusGetCurrent focus of
        Just FocusNoSession ->
          Tmux.create (editorVal editor) Nothing
        _ -> pure ()

runSwitch :: MonadIO m => Model -> m ()
runSwitch = traverse_ (Tmux.switch . snd) . focusedTmuxType

runMerge :: MonadIO m => Model -> m ()
runMerge m@Model {windows} =
  case focusedTmuxType m of
    Just (_, Tmux.TmuxWindow source) ->
      case dropWhile ((/=) (windowId source) . windowId) windows of
        _ : target : _ -> Tmux.merge source target
        _ -> pure ()
    _ -> pure ()

data Direction = MoveUp | MoveDown

runMove :: MonadIO m => Direction -> Model -> m ()
runMove dir m@Model {windows} =
  case focusedTmuxType m of
    Just (_, Tmux.TmuxWindow source) ->
      let ws = case dir of
            MoveDown -> windows
            MoveUp -> reverse windows
       in case dropWhile ((/=) (windowId source) . windowId) ws of
            _ : target : _ -> Tmux.swap source target
            _ -> pure ()
    _ -> pure ()

runKill :: MonadIO m => Model -> m ()
runKill = traverse_ (Tmux.kill . snd) . focusedTmuxType

handlePersist :: Model -> Brick.EventM Name (Brick.Next Model)
handlePersist m@Model {storedSessions} =
  case focusedTmuxType m of
    Just (_, Tmux.TmuxSession session) ->
      case find ((==) (sessionTitle session) . sessionTitle . Tmux.Stored.session) storedSessions of
        Just _ ->
          Brick.continue
            m
              { mode =
                  Confirm
                    ConfirmData
                      { action = Callback_ $ \_ -> Tmux.Stored.persist session,
                        message = "Do you want to overwrite existing stored session?",
                        dialog = yesNoDialog YesNoDialog {title = sessionTitle session, yes = "Store", no = "Cancel"}
                      }
              }
        Nothing -> Tmux.Stored.persist session >> refresh m
    _ -> Brick.continue m

focusedTmuxType :: Model -> Maybe (Text, Tmux.TmuxType)
focusedTmuxType Model {..} =
  ((,) (editorVal editor))
    <$> case Focus.focusGetCurrent focus of
      Just (FocusWindow id) -> Tmux.TmuxWindow <$> find ((==) id . windowId) windows
      Just (FocusSession id) -> Tmux.TmuxSession <$> find ((==) id . sessionId) sessions
      Just (FocusPane id) -> Tmux.TmuxPane <$> find ((==) id . paneId) panes
      _ -> Nothing

editorVal :: Edit.Editor Text Name -> Text
editorVal = mconcat . Edit.getEditContents

data TmuxTuiException = NotImplementedYet
  deriving (Show)

instance E.Exception TmuxTuiException

handleCharPress :: Model -> Char -> [Vty.Modifier] -> Brick.EventM Name (Brick.Next Model)
handleCharPress m ' ' [] = handleSpace m
handleCharPress m '/' [] = handleSearch m & Brick.continue
handleCharPress m 'r' [] = handleRename m & refresh
handleCharPress m 'c' [] = handleCreate m >>= Brick.continue
handleCharPress m 'd' [] = handleDelete m & refresh
handleCharPress m 's' [] = handlePersist m
handleCharPress m 'k' [] = handleMoveUpDown Focus.focusPrev m & refresh
handleCharPress m 'j' [] = handleMoveUpDown Focus.focusNext m & refresh
handleCharPress m 'h' [] = handleMoveLeft m & refresh
handleCharPress m 'l' [] = handleMoveRight m & refresh
handleCharPress m 'g' [] = handleMoveToTop m & refresh
handleCharPress m 'G' [] = handleMoveToBottom m & refresh
handleCharPress m 'm' [] = runMerge m >> refresh m
handleCharPress m 'K' [] = runMove MoveUp m >> refresh m
handleCharPress m 'J' [] = runMove MoveDown m >> refresh m
handleCharPress m 'y' [] = handleYank m & Brick.continue
handleCharPress m 'p' [] = handlePaste m >>= refresh
handleCharPress m 'q' [] = Brick.halt m
handleCharPress m 'c' [Vty.MCtrl] = Brick.halt m
handleCharPress m _ _ = Brick.continue m

handleYank :: Model -> Model
handleYank m@Model {..} =
  m
    { moving = case Focus.focusGetCurrent focus of
        Just (FocusWindow id) ->
          MovingWindow <$> find ((==) id . windowId) windows
        Just (FocusPane id) ->
          MovingPane <$> find ((==) id . paneId) panes
        _ -> Nothing
    }

handlePaste :: MonadIO m => Model -> m Model
handlePaste m@Model {..} =
  let source =
        case moving of
          Just (MovingWindow Window {..}) ->
            Just (Tmux.MoveWindow, unSessionId windowSessionId <> ":" <> unWindowId windowId)
          Just (MovingPane Pane {..}) ->
            Just (Tmux.MovePane, unSessionId paneSessionId <> ":" <> unWindowId paneWindowId <> "." <> unPaneId paneId)
          _ -> Nothing
      destination = case Focus.focusGetCurrent focus of
        Just (FocusSession id) -> Just (unSessionId id <> ":")
        Just (FocusWindow id) ->
          case find ((==) id . windowId) windows of
            Just Window {windowSessionId} ->
              Just (unSessionId windowSessionId <> ":" <> unWindowId id)
            Nothing -> Nothing
        Just (FocusPane id) ->
          case find ((==) id . paneId) panes of
            Just Pane {paneSessionId, paneWindowId} ->
              Just (unSessionId paneSessionId <> ":" <> unWindowId paneWindowId <> "." <> unPaneId id)
            Nothing -> Nothing
        _ -> Nothing
   in do
        case (source, destination) of
          (Just (move, s), Just d) -> Tmux.move move s d
          _ -> pure ()
        pure m {moving = Nothing}

handleSearch :: Model -> Model
handleSearch m@Model {..} =
  let mkSearchEditor Nothing = Edit.applyEdit (\_ -> TZ.gotoEOL $ TZ.textZipper [""] Nothing) searchEditor
      mkSearchEditor (Just val) = Edit.applyEdit (\_ -> TZ.gotoEOL $ TZ.textZipper [val] Nothing) searchEditor
   in case Focus.focusGetCurrent focus of
        Just (StoredSessionTitle _) -> m {mode = Search SearchStored, searchEditor = mkSearchEditor filterStored}
        Just (FocusSession _) -> m {mode = Search SearchSession, searchEditor = mkSearchEditor filterSession}
        Just (FocusWindow _) -> m {mode = Search SearchWindow, searchEditor = mkSearchEditor filterWindow}
        Just (FocusPane _) -> m {mode = Search SearchPane, searchEditor = mkSearchEditor filterPane}
        _ -> m

handleSpace :: Model -> Brick.EventM Name (Brick.Next Model)
handleSpace m =
  case Focus.focusGetCurrent $ focus m of
    Just (StoredSessionTitle t) -> do
      -- get a petname for new session
      case find ((==) t . sessionTitle) $ sessions m of
        Just _ -> do
          name <- liftIO $ Petname.random "-"
          Brick.continue
            m
              { mode =
                  RequireInput
                    InputData
                      { inputCallback = Callback $ \m@Model {editor} ->
                          do
                            handleInstantiate m t (Just (editorVal editor)),
                        inputDialog =
                          yesNoDialog
                            YesNoDialog
                              { title = "There is already a session with this name.",
                                yes = "Create",
                                no = "Cancel"
                              }
                      },
                editor = Edit.applyEdit (\_ -> TZ.gotoEOL $ TZ.textZipper [name] Nothing) $ editor m
              }
        Nothing -> liftIO (handleInstantiate m t Nothing) >>= Brick.continue
    Just _ ->
      runSwitch m >> Brick.halt m
    Nothing -> Brick.continue m

handleInstantiate :: Model -> Text -> Maybe Text -> IO Model
handleInstantiate m t maybeNewTitle =
  case find ((==) t . Tmux.sessionTitle . Tmux.Stored.session) $ storedSessions m of
    Just storedSession -> do
      _ <- Tmux.Stored.instantiate storedSession title
      newM@Model {sessions, focus} <- reloadRefocus m
      case find ((==) title . sessionTitle) sessions of
        Just Session {sessionId} ->
          updateWindows newM {focus = Focus.focusSetCurrent (FocusSession sessionId) focus}
        Nothing -> pure newM
    Nothing -> pure m
  where
    title = M.fromMaybe t maybeNewTitle

handleMoveLeft :: Model -> Model
handleMoveLeft m@Model {..} =
  case Focus.focusGetCurrent focus of
    Just (FocusPane id) ->
      case find ((==) id . paneId) panes of
        Nothing -> m
        Just Pane {paneWindowId} ->
          m {focus = Focus.focusSetCurrent (FocusWindow paneWindowId) focus, filterPane = Nothing}
    Just (FocusWindow id) ->
      case find ((==) id . windowId) windows of
        Nothing -> m
        Just Window {windowSessionId} ->
          m {focus = Focus.focusSetCurrent (FocusSession windowSessionId) focus, filterWindow = Nothing}
    Just (FocusSession id) ->
      case find ((==) id . sessionId) sessions of
        Nothing -> m
        Just s ->
          case find ((==) (sessionTitle s) . sessionTitle . Tmux.Stored.session) storedSessions
            <|> head storedSessions of
            Nothing -> m
            Just storedSession ->
              m {focus = Focus.focusSetCurrent (StoredSessionTitle . sessionTitle $ Tmux.Stored.session storedSession) focus}
    Just FocusNoSession ->
      case head storedSessions of
        Nothing -> m
        Just storedSession ->
          m {focus = Focus.focusSetCurrent (StoredSessionTitle . sessionTitle $ Tmux.Stored.session storedSession) focus}
    Just (StoredSessionTitle title) ->
      case find ((==) title . sessionTitle) sessions <|> head sessions of
        Nothing -> m
        Just Session {sessionId} ->
          case find ((==) sessionId . windowSessionId) windows <|> head windows of
            Nothing -> m {focus = Focus.focusSetCurrent FocusNoSession focus}
            Just Window {windowId} ->
              m {focus = Focus.focusSetCurrent (FocusWindow windowId) focus}
    _ -> m

handleMoveRight :: Model -> Model
handleMoveRight m@Model {..} =
  case Focus.focusGetCurrent focus of
    Just (FocusPane id) ->
      case find ((==) id . paneId) panes of
        Nothing -> m
        Just Pane {paneSessionTitle} ->
          case find ((==) paneSessionTitle . sessionTitle . Tmux.Stored.session) storedSessions
            <|> head storedSessions of
            Nothing -> m
            Just storedSession ->
              m {focus = Focus.focusSetCurrent (StoredSessionTitle . sessionTitle . Tmux.Stored.session $ storedSession) focus, filterWindow = Nothing, filterPane = Nothing}
    Just (FocusWindow id) ->
      case find ((==) id . windowId) windows of
        Nothing -> m
        Just Window {windowId} ->
          case find ((==) windowId . paneWindowId) panes of
            Nothing -> m
            Just Pane {paneId} ->
              m {focus = Focus.focusSetCurrent (FocusPane paneId) focus}
    Just (FocusSession id) ->
      case find ((==) id . sessionId) sessions of
        Nothing -> m
        Just Session {sessionId} ->
          case find ((==) sessionId . windowSessionId) windows of
            Nothing -> m
            Just Window {windowId} ->
              m {focus = Focus.focusSetCurrent (FocusWindow windowId) focus}
    Just FocusNoSession ->
      case head storedSessions of
        Nothing -> m
        Just storedSession ->
          m {focus = Focus.focusSetCurrent (StoredSessionTitle . sessionTitle $ Tmux.Stored.session storedSession) focus}
    Just (StoredSessionTitle title) ->
      case find ((==) title . sessionTitle) sessions <|> head sessions of
        Nothing -> m {focus = Focus.focusSetCurrent FocusNoSession focus}
        Just Session {sessionId} ->
          m {focus = Focus.focusSetCurrent (FocusSession sessionId) focus}
    _ -> m

handleMoveUpDown :: (FocusRing -> FocusRing) -> Model -> Model
handleMoveUpDown move m@Model {focus} =
  m {focus = moveUntil (not . differentType focus) move $ move focus}

handleMoveToBottom :: Model -> Model
handleMoveToBottom m@Model {focus} =
  m
    { focus =
        Focus.focusPrev
          $ moveUntil (differentType focus) Focus.focusNext
          $ Focus.focusNext focus
    }

handleMoveToTop :: Model -> Model
handleMoveToTop m@Model {focus} =
  m
    { focus =
        Focus.focusNext
          $ moveUntil (differentType focus) Focus.focusPrev
          $ Focus.focusPrev focus
    }

handleRename :: Model -> Model
handleRename m@Model {focus, windows, sessions, editor} =
  let mkMode title =
        RequireInput
          InputData
            { inputCallback = Callback_ runRename,
              inputDialog = yesNoDialog YesNoDialog {title, yes = "Save", no = "Cancel"}
            }
   in case Focus.focusGetCurrent focus of
        Just (FocusWindow id) ->
          case find ((==) id . windowId) windows of
            Just Window {windowTitle} ->
              m
                { mode = mkMode "Renaming Window",
                  editor = Edit.applyEdit (\_ -> TZ.gotoEOL $ TZ.textZipper [windowTitle] Nothing) editor
                }
            Nothing -> m
        Just (FocusSession id) ->
          case find ((==) id . sessionId) sessions of
            Just Session {sessionTitle} ->
              m
                { mode = mkMode "Renaming Session",
                  editor = Edit.applyEdit (\_ -> TZ.gotoEOL $ TZ.textZipper [sessionTitle] Nothing) editor
                }
            Nothing -> m
        _ -> m

handleDelete :: Model -> Model
handleDelete m@Model {..} =
  let mkMode title message =
        Confirm
          ConfirmData
            { action = Callback_ runKill,
              message,
              dialog = yesNoDialog YesNoDialog {title, yes = "Delete", no = "Cancel"}
            }
   in M.fromMaybe m $
        case Focus.focusGetCurrent focus of
          Just (FocusWindow id) ->
            ( \Window {windowTitle} ->
                m {mode = mkMode windowTitle "Do you want to delete this window?"}
            )
              <$> find ((==) id . windowId) windows
          Just (FocusPane id) ->
            ( \Pane {paneNumber} ->
                m {mode = mkMode (show paneNumber) "Do you want to delete this pane?"}
            )
              <$> find ((==) id . paneId) panes
          Just (FocusSession id) ->
            ( \Session {sessionTitle} ->
                m {mode = mkMode sessionTitle "Do you want to delete this session?"}
            )
              <$> find ((==) id . sessionId) sessions
          Just (StoredSessionTitle id) ->
            ( \Tmux.Stored.Session {Tmux.Stored.session} ->
                m
                  { mode =
                      Confirm
                        ConfirmData
                          { action = Callback_ . const $ Tmux.Stored.delete id,
                            message = "Do you want to delete this stored session?",
                            dialog = yesNoDialog YesNoDialog {title = sessionTitle session, yes = "Delete", no = "Cancel"}
                          }
                  }
            )
              <$> find ((==) id . sessionTitle . Tmux.Stored.session) storedSessions
          _ -> Just m

onConfirm :: MonadIO m => Callback -> Model -> m Model
onConfirm (Callback f) m = liftIO $ do
  newM <- f m
  pure (handleMoveUpDown Focus.focusPrev newM {mode = Normal})
onConfirm (Callback_ f) m = liftIO $ do
  _ <- f m
  pure (handleMoveUpDown Focus.focusPrev m {mode = Normal})

handleCreate :: MonadIO m => Model -> m Model
handleCreate m@Model {focus, windows, sessions, editor} = do
  name <- liftIO $ Petname.random "-"
  pure $ case Focus.focusGetCurrent focus of
    Just (FocusWindow _) ->
      m
        { mode =
            RequireInput
              InputData
                { inputCallback = Callback_ runCreate,
                  inputDialog = yesNoDialog YesNoDialog {title = "Creating Window", yes = "Save", no = "Cancel"}
                },
          editor = Edit.applyEdit (\_ -> TZ.gotoEOL $ TZ.textZipper [name] Nothing) editor
        }
    Just (FocusSession _) ->
      m
        { mode =
            RequireInput
              InputData
                { inputCallback = Callback_ runCreate,
                  inputDialog = yesNoDialog YesNoDialog {title = "Creating Session", yes = "Save", no = "Cancel"}
                },
          editor = Edit.applyEdit (\_ -> TZ.gotoEOL $ TZ.textZipper [name] Nothing) editor
        }
    Just FocusNoSession ->
      m
        { mode =
            RequireInput
              InputData
                { inputCallback = Callback_ runCreate,
                  inputDialog = yesNoDialog YesNoDialog {title = "Creating Session", yes = "Save", no = "Cancel"}
                },
          editor = Edit.applyEdit (\_ -> TZ.gotoEOL $ TZ.textZipper [name] Nothing) editor
        }
    _ -> m

updateWindows :: MonadIO m => Model -> m Model
updateWindows m = do
  case currentSessionId m of
    Nothing -> pure m
    Just sessionId -> do
      windows <- Tmux.listWindows sessionId
      pure m {windows}

updatePanes :: MonadIO m => Model -> m Model
updatePanes m@Model {..} = do
  case Focus.focusGetCurrent focus of
    Just (FocusSession sessionId) -> do
      panes <- Tmux.panesForSession sessionId
      pure m {panes}
    Just (FocusPane id) ->
      case find ((==) id . paneId) panes of
        Nothing -> pure m {panes = []}
        Just Pane {paneSessionId, paneWindowId} -> do
          panes <- Tmux.panesForWindow paneSessionId paneWindowId
          pure m {panes}
    Just (FocusWindow windowId) ->
      case find ((==) windowId . Tmux.windowId) windows of
        Nothing -> pure m {panes = []}
        Just Window {windowSessionId} -> do
          panes <- Tmux.panesForWindow windowSessionId windowId
          pure m {panes}
    Just (StoredSessionTitle sessionTitle) ->
      case find ((==) sessionTitle . Tmux.sessionTitle . Tmux.Stored.session) storedSessions of
        Just Tmux.Stored.Session {..} -> pure m {panes}
        Nothing -> pure m {panes = []}
    _ -> pure m {panes = []}

currentSessionId :: Model -> Maybe SessionId
currentSessionId m@Model {..} =
  case Focus.focusGetCurrent focus of
    Just (FocusSession id) -> Just id
    Just (FocusPane id) ->
      case find ((==) id . paneId) panes of
        Nothing -> Nothing
        Just Pane {paneSessionId} -> Just paneSessionId
    Just (FocusWindow id) ->
      case find ((==) id . windowId) windows of
        Nothing -> Nothing
        Just Window {windowSessionId} -> Just windowSessionId
    _ -> Nothing

currentWindowId :: Model -> Maybe WindowId
currentWindowId m@Model {..} =
  case Focus.focusGetCurrent focus of
    Just (FocusPane id) ->
      case find ((==) id . paneId) panes of
        Nothing -> Nothing
        Just Pane {paneWindowId} -> Just paneWindowId
    Just (FocusWindow id) -> Just id
    _ -> Nothing

refresh :: Model -> Brick.EventM Name (Brick.Next Model)
refresh m = Brick.continue =<< reloadRefocus m

reloadRefocus :: MonadIO m => Model -> m Model
reloadRefocus m = updateFocus <$> reload m

reload :: MonadIO m => Model -> m Model
reload m = do
  currentTime <- liftIO Time.getCurrentTime
  storedSessions <- Tmux.Stored.list
  sessions <- Tmux.listSessions
  updatePanes =<< updateWindows m {sessions, storedSessions, currentTime}

updateFocus :: Model -> Model
updateFocus m =
  let filtered@Model {..} = filterItems m
      newFocusRing =
        Focus.focusRing $
          (paneToName <$> panes)
            ++ (windowToName <$> windows)
            ++ (sessionToName <$> sessions)
            ++ (storedSessionToName <$> storedSessions)
            ++ ( case sessions of
                   [] -> [FocusNoSession]
                   _ -> []
               )
   in filterItems $ case Focus.focusGetCurrent focus of
        Just current -> filtered {focus = Focus.focusSetCurrent current newFocusRing}
        Nothing -> filtered {focus = newFocusRing}

filterItems :: Model -> Model
filterItems m@Model {..} =
  m
    { storedSessions = case filterBy filterStored storedSessions $ Tmux.sessionTitle . Tmux.Stored.session of
        [] -> storedSessions
        xs -> xs,
      sessions = case filterBy filterSession sessions Tmux.sessionTitle of
        [] -> sessions
        xs -> xs,
      windows = case filterBy filterWindow windows Tmux.windowTitle of
        [] -> windows
        xs -> xs,
      panes = case filterBy filterPane panes Tmux.paneCurrentCommand of
        [] -> panes
        xs -> xs
    }
  where
    filterBy Nothing xs _ = xs
    filterBy (Just filter) xs f = Fuzzy.original <$> Fuzzy.filter filter xs "" "" f False

-- Drawing

view :: Model -> [Brick.Widget Name]
view model@Model {..} =
  [ maybeEditor mode editor focus,
    maybeConfirmation mode,
    ( viewStoredSessions model
        <+> viewSessions model
        <+> viewWindows mode focus searchEditor filterWindow ws
        <+> viewPanes model
    )
      <=> (footer model <+> pasteMode)
  ]
  where
    pasteMode =
      Brick.modifyDefAttr modReverse
        $ Brick.withAttr attentionAttr
        $ case moving of
          Just (MovingWindow Window {..}) -> Brick.txt ("Pasting Window " <> windowTitle)
          Just (MovingPane Pane {..}) -> Brick.txt ("Pasting Pane #" <> show paneNumber)
          Nothing -> Brick.txt ""
    ws =
      case Focus.focusGetCurrent focus of
        Just (StoredSessionTitle t) ->
          case find ((==) t . sessionTitle . Tmux.Stored.session) storedSessions of
            Just storedSession -> Tmux.Stored.windows storedSession
            _ -> windows
        _ -> windows

streched :: Maybe Int -> Brick.Widget Name -> Brick.Widget Name
streched x =
  ( case x of
      Just x' -> Brick.hLimitPercent x'
      Nothing -> identity
  )
    . Brick.padRight Brick.Max
    . Brick.padBottom Brick.Max

maybeEditor :: Mode -> Edit.Editor Text Name -> FocusRing -> Brick.Widget Name
maybeEditor (RequireInput InputData {inputDialog}) e f = editorWidget inputDialog e f
maybeEditor _ _ _ = Brick.txt ""

maybeConfirmation :: Mode -> Brick.Widget Name
maybeConfirmation (Confirm d) = confirmationWidget d
maybeConfirmation _ = Brick.txt ""

editorWidget :: Dialog.Dialog YesNo -> Edit.Editor Text Name -> FocusRing -> Brick.Widget Name
editorWidget inputDialog editor focus =
  Brick.overrideAttr Border.borderAttr attentionAttr
    $ Dialog.renderDialog inputDialog
    $ box "" False
    $ Brick.vLimit 1
    $ renderEditor editor focus

renderEditor :: Edit.Editor Text Name -> FocusRing -> Brick.Widget Name
renderEditor editor focus =
  Focus.withFocusRing focus (Edit.renderEditor contentWithCursor) editor
  where
    contentWithCursor t =
      let (_, cursorPos) = TZ.cursorPosition (editor ^. Edit.editContentsL)
          (before, after) = T.splitAt cursorPos $ mconcat t
       in Brick.hBox $
            Brick.txt before
              : case T.uncons after of
                Just (x, rest) -> [Brick.modifyDefAttr modReverse $ Brick.txt $ T.singleton x, Brick.txt rest]
                Nothing -> [Brick.modifyDefAttr modReverse $ Brick.txt " ", Brick.txt after]

confirmationWidget :: ConfirmData -> Brick.Widget Name
confirmationWidget ConfirmData {dialog, message} =
  Brick.overrideAttr Border.borderAttr attentionAttr $
    Dialog.renderDialog dialog (Brick.txt message)

data YesNoDialog
  = YesNoDialog
      { title :: Text,
        yes :: Text,
        no :: Text
      }

yesNoDialog :: YesNoDialog -> Dialog.Dialog YesNo
yesNoDialog YesNoDialog {title, yes, no} =
  Dialog.dialog
    (Just $ T.unpack title)
    ( Just (0, [(T.unpack yes, Yes), (T.unpack no, No)])
    )
    50

viewSessions :: Model -> Brick.Widget Name
viewSessions Model {..} =
  ( case sessions of
      [] ->
        [ Brick.txt "No started sessions"
            & Brick.modifyDefAttr modDim
            & Brick.clickable FocusNoSession
        ]
      xs ->
        viewSearch SearchSession mode focus searchEditor filterSession
          : (viewSession focus <$> xs)
  )
    & Brick.vBox
    & streched (Just 15)
    & box "Sessions" (sessionFocused focus)

viewSession :: FocusRing -> Session -> Brick.Widget Name
viewSession focus Session {sessionId, sessionTitle, isAttached} =
  styleCurrent
    (FocusSession sessionId)
    focus
    ( Brick.txt sessionTitle
        & Brick.padRight (Brick.Pad 1)
        & Brick.clickable (FocusSession sessionId)
        & isActiveToAttrName isAttached
    )

viewStoredSessions :: Model -> Brick.Widget Name
viewStoredSessions Model {..} =
  ( case storedSessions of
      [] -> [Brick.txt "No stored sessions" & Brick.modifyDefAttr modDim]
      xs ->
        viewSearch SearchStored mode focus searchEditor filterStored
          : (viewStoredSession focus currentTime <$> xs)
  )
    & Brick.vBox
    & streched (Just 15)
    & box "Stored" (storedSessionFocused focus)

viewSearch :: SearchTarget -> Mode -> FocusRing -> Edit.Editor Text Name -> Maybe Text -> Brick.Widget Name
viewSearch x (Search y) focus editor maybeFilter
  | x == y =
    Brick.hBox
      [Brick.txt "/" & Brick.withAttr activeAttr, Brick.vLimit 1 $ renderEditor editor focus]
  | otherwise = viewFilter maybeFilter
viewSearch _ _ _ _ maybeFilter = viewFilter maybeFilter

viewFilter :: Maybe Text -> Brick.Widget Name
viewFilter Nothing = Brick.txt ""
viewFilter (Just filter) = Brick.hBox [Brick.txt "/" & Brick.withAttr attentionAttr, Brick.txt filter]

viewStoredSession :: FocusRing -> Time.UTCTime -> Tmux.Stored.Session -> Brick.Widget Name
viewStoredSession focus currentTime Tmux.Stored.Session {Tmux.Stored.session = Session {sessionTitle}, Tmux.Stored.lastModifiedAt} =
  info <+> label
  where
    label =
      styleCurrent
        (StoredSessionTitle sessionTitle)
        focus
        ( Brick.txt sessionTitle
            & Brick.padRight (Brick.Pad 1)
            & Brick.clickable (StoredSessionTitle sessionTitle)
        )
    info =
      ( if  | delta < 60 * 60 -> show (delta `divInt` 60) ++ "m"
            | delta < 24 * 60 * 60 -> show (delta `divInt` (60 * 60)) ++ "h"
            | delta < 7 * 24 * 60 * 60 -> show (delta `divInt` (24 * 60 * 60)) ++ "d"
            | otherwise -> ">7d"
      )
        & T.pack
        & T.justifyLeft 4 ' '
        & Brick.txt
        & Brick.modifyDefAttr modDim
    delta :: Int
    delta =
      Time.diffUTCTime currentTime lastModifiedAt
        & Time.nominalDiffTimeToSeconds
        & properFraction
        & \(a, _) -> a

viewWindows :: Mode -> FocusRing -> Edit.Editor Text Name -> Maybe Text -> [Window] -> Brick.Widget Name
viewWindows mode focus searchEditor filter windows =
  ( case windows of
      [] -> [Brick.txt "no windows" & Brick.modifyDefAttr modDim]
      xs ->
        viewSearch SearchWindow mode focus searchEditor filter
          : (viewWindow mode focus <$> xs)
  )
    & Brick.vBox
    & Brick.padBottom Brick.Max
    & Brick.padRight Brick.Max
    & box title (windowFocused focus)
  where
    title =
      case sessionName <$> head windows of
        Just t -> "Windows in " <> t
        Nothing -> "Windows"

isActiveToAttrName :: IsActive -> Brick.Widget n -> Brick.Widget n
isActiveToAttrName isActive w =
  case isActive of
    Active ->
      Brick.withAttr activeAttr w
    Inactive ->
      w

viewWindow :: Mode -> FocusRing -> Window -> Brick.Widget Name
viewWindow mode focus Window {windowId, number, windowTitle, isActive, currentCommand, currentPath} =
  styleCurrent (FocusWindow windowId) focus label <+> info
  where
    label =
      Brick.txt (T.concat [show number, ":", " ", windowTitle])
        & Brick.clickable (FocusWindow windowId)
        & case Focus.focusGetCurrent focus of
          Just (StoredSessionTitle t) -> identity
          _ -> isActiveToAttrName isActive
    info = Brick.txt (T.concat [" ", currentCommand, " ", currentPath]) & Brick.modifyDefAttr modDim

viewPanes :: Model -> Brick.Widget Name
viewPanes m@Model {..} =
  ( case panes of
      [] -> [Brick.txt "No window selected" & Brick.modifyDefAttr modDim]
      x : xs ->
        viewSearch SearchPane mode focus searchEditor filterPane
          : viewPanePerWindow focus Nothing windows x xs
  )
    & Brick.vBox
    & streched Nothing
    & box title (paneFocused focus)
  where
    title =
      case Focus.focusGetCurrent focus of
        Just (FocusSession sessionId) ->
          case find ((==) sessionId . Tmux.sessionId) sessions of
            Just Session {sessionTitle} -> "Panes in Session " <> sessionTitle
            Nothing -> "Panes"
        Just (FocusWindow windowId) ->
          case find ((==) windowId . Tmux.windowId) windows of
            Just Window {windowTitle} -> "Panes in Window " <> windowTitle
            Nothing -> "Panes"
        Just (StoredSessionTitle sessionTitle) ->
          "Panes in Stored Session " <> sessionTitle
        _ -> "Panes"

viewPanePerWindow :: FocusRing -> Maybe WindowId -> [Window] -> Pane -> [Pane] -> [Brick.Widget Name]
viewPanePerWindow focus maybeWindowId windows pane@Pane {paneWindowId} rest =
  ( if maybeWindowId == Just paneWindowId
      then Brick.txt ""
      else
        let maybeWindow = find ((==) paneWindowId . windowId) windows
         in (Brick.modifyDefAttr modBold $ Brick.txt $ M.fromMaybe "" $ windowTitle <$> maybeWindow)
  )
    : viewPane focus pane
    : case rest of
      [] -> []
      next : rest' -> viewPanePerWindow focus (Just paneWindowId) windows next rest'

viewPane :: FocusRing -> Pane -> Brick.Widget Name
viewPane focus Pane {..} =
  styleCurrent (FocusPane paneId) focus label <+> info
  where
    label = Brick.txt (T.concat [show paneNumber, ": ", paneCurrentCommand])
      & Brick.clickable (FocusPane paneId)
      & case Focus.focusGetCurrent focus of
        Just (StoredSessionTitle t) -> identity
        _ -> isActiveToAttrName paneIsActive
    info = Brick.txt (T.concat [" ", paneCurrentPath]) & Brick.modifyDefAttr modDim

box :: Text -> Bool -> Brick.Widget Name -> Brick.Widget Name
box title focused w =
  if focused
    then
      Brick.overrideAttr Border.borderAttr activeAttr
        $ Brick.withBorderStyle Border.unicode
        $ Border.borderWithLabel (Brick.txt title & Brick.withAttr activeAttr) w
    else
      Brick.withBorderStyle Border.unicode $
        Border.borderWithLabel (Brick.txt title) w

activeAttr, attentionAttr, helpAttr :: Brick.AttrName
activeAttr = "activeAttr"
attentionAttr = "attentionAttr"
helpAttr = "helpAttr"

theMap :: Brick.AttrMap
theMap =
  Brick.attrMap
    Vty.defAttr
    [ (activeAttr, Brick.fg Vty.cyan),
      (attentionAttr, Brick.fg Vty.magenta),
      (helpAttr, Brick.fg Vty.blue),
      (Dialog.buttonSelectedAttr, Vty.withStyle Vty.defAttr Vty.reverseVideo `Vty.withForeColor` Vty.magenta)
    ]

styleCurrent :: Name -> FocusRing -> Brick.Widget Name -> Brick.Widget Name
styleCurrent id focus =
  if isCurrent id focus
    then Brick.modifyDefAttr modReverse
    else identity

modReverse :: Vty.Attr -> Vty.Attr
modReverse attr = Vty.withStyle attr Vty.reverseVideo

modDim :: Vty.Attr -> Vty.Attr
modDim attr = Vty.withStyle attr Vty.dim

modBold :: Vty.Attr -> Vty.Attr
modBold attr = Vty.withStyle attr Vty.bold

paneToName :: Pane -> Name
paneToName = FocusPane . paneId

windowToName :: Window -> Name
windowToName = FocusWindow . windowId

sessionToName :: Session -> Name
sessionToName = FocusSession . sessionId

storedSessionToName :: Tmux.Stored.Session -> Name
storedSessionToName = StoredSessionTitle . sessionTitle . Tmux.Stored.session

isCurrent :: Name -> FocusRing -> Bool
isCurrent n focus =
  case Focus.focusGetCurrent focus of
    Just x -> x == n
    Nothing -> False

differentType :: FocusRing -> FocusRing -> Bool
differentType a b =
  case (Focus.focusGetCurrent a, Focus.focusGetCurrent b) of
    (Just (FocusPane _), Just (FocusWindow _)) -> True
    (Just (FocusPane _), Just (FocusSession _)) -> True
    (Just (FocusPane _), Just (StoredSessionTitle _)) -> True
    (Just (FocusWindow _), Just (FocusPane _)) -> True
    (Just (FocusWindow _), Just (FocusSession _)) -> True
    (Just (FocusWindow _), Just (StoredSessionTitle _)) -> True
    (Just (FocusSession _), Just (FocusPane _)) -> True
    (Just (FocusSession _), Just (FocusWindow _)) -> True
    (Just (FocusSession _), Just (StoredSessionTitle _)) -> True
    (Just (StoredSessionTitle _), Just (FocusPane _)) -> True
    (Just (StoredSessionTitle _), Just (FocusWindow _)) -> True
    (Just (StoredSessionTitle _), Just (FocusSession _)) -> True
    _ -> False

activeIsFocused :: Model -> FocusRing -> Bool
activeIsFocused Model {sessions, windows} focus =
  case Focus.focusGetCurrent focus of
    Just (FocusSession x) -> (sessionId <$> find ((==) Active . isAttached) sessions) == Just x
    Just (FocusWindow x) -> (windowId <$> find ((==) Active . isActive) windows) == Just x
    _ -> False

sessionFocused :: FocusRing -> Bool
sessionFocused focus =
  case Focus.focusGetCurrent focus of
    Just (FocusSession _) -> True
    Just FocusNoSession -> True
    _ -> False

storedSessionFocused :: FocusRing -> Bool
storedSessionFocused focus =
  case Focus.focusGetCurrent focus of
    Just (StoredSessionTitle _) ->
      True
    _ -> False

windowFocused :: FocusRing -> Bool
windowFocused focus =
  case Focus.focusGetCurrent focus of
    Just (FocusWindow _) -> True
    _ -> False

paneFocused :: FocusRing -> Bool
paneFocused focus =
  case Focus.focusGetCurrent focus of
    Just (FocusPane _) -> True
    _ -> False

moveUntil :: (FocusRing -> Bool) -> (FocusRing -> FocusRing) -> FocusRing -> FocusRing
moveUntil pred move focus =
  if pred focus
    then focus
    else moveUntil pred move (move focus)

updateWhen :: Eq b => b -> (a -> b) -> (a -> a) -> [a] -> [a]
updateWhen _ _ _ [] = []
updateWhen x toX f (first : rest) =
  if x == toX first
    then f first : rest
    else first : updateWhen x toX f rest

footer :: Model -> Brick.Widget Name
footer model@Model {..} = Brick.padRight Brick.Max $
  case mode of
    Normal -> currentActionHelp model
    RequireInput _ ->
      Brick.withAttr helpAttr
        $ Brick.txt
        $ "enter: Save, esc: Cancel"
    Confirm _ ->
      Brick.withAttr helpAttr
        $ Brick.txt
        $ "space/enter: Confirm, esc: Cancel"
    Search _ ->
      Brick.withAttr helpAttr
        $ Brick.txt
        $ "enter: Keep filter, esc: Remove filter"

currentActionHelp :: Model -> Brick.Widget Name
currentActionHelp Model {..} =
  Brick.withAttr helpAttr
    $ Brick.txtWrap
    $ case Focus.focusGetCurrent focus of
      Just (FocusPane _) ->
        T.intercalate
          ", "
          [ case (moving, filterPane) of
              (_, Just _) -> "/: Search, esc: Clear filter"
              (Just (MovingWindow _), _) -> "/: Search, esc: Stop Pasting"
              _ -> "/: Search, esc: Exit"
          ]
      Just (FocusWindow _) ->
        T.intercalate
          ", "
          [ "space: Select",
            "r: Rename",
            "c: Create",
            "d: Delete",
            case (moving, filterWindow) of
              (_, Just _) -> "/: Search, esc: Clear filter"
              (Just (MovingWindow _), _) -> "/: Search, esc: Stop Pasting"
              _ -> "/: Search, esc: Exit"
          ]
      Just (FocusSession _) ->
        T.intercalate
          ", "
          [ "space: Attach",
            "r: Rename",
            "c: Create",
            "d: Delete",
            "s: Store",
            case filterSession of
              Just _ -> "/: Search, esc: Clear filter"
              Nothing -> "/: Search, esc: Exit"
          ]
      Just FocusNoSession ->
        "c: Create"
      Just (StoredSessionTitle _) ->
        T.intercalate
          ", "
          [ "space: Instantiate",
            "d: Delete",
            case filterStored of
              Just _ -> "/: Search, esc: Clear filter"
              Nothing -> "/: Search, esc: Exit"
          ]
      _ -> "esc: Exit"
  where
    stopPasting = "esc: Stop Pasting"
