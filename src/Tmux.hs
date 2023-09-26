{-# LANGUAGE MultiWayIf #-}

module Tmux
  ( Session (..),
    SessionId (..),
    Window (..),
    WindowId (..),
    Pane (..),
    PaneId (..),
    listSessions,
    listWindows,
    panesForSession,
    panesForWindow,
    IsActive (..),
    -- Actions
    TmuxType (..),
    rename,
    create,
    switch,
    kill,
    merge,
    swap,
    Command (..),
    runCommand,
    move,
    Move (..),
  )
where

import qualified Cmd
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as P
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Protolude hiding (swap)
import System.Environment (getEnv)
import System.Exit (ExitCode (..))

data Session
  = Session
      { sessionId :: SessionId,
        sessionTitle :: Text,
        isAttached :: IsActive
      }
  deriving (Generic)

instance Aeson.ToJSON Session

instance Aeson.FromJSON Session

newtype SessionId = SessionId {unSessionId :: Text}
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON SessionId

instance Aeson.FromJSON SessionId

data Window
  = Window
      { windowId :: WindowId,
        windowSessionId :: SessionId,
        number :: Int,
        windowTitle :: Text,
        isActive :: IsActive,
        currentCommand :: Text,
        currentPath :: Text,
        sessionName :: Text,
        layout :: Text
      }
  deriving (Generic)

instance Aeson.ToJSON Window

instance Aeson.FromJSON Window

newtype WindowId = WindowId {unWindowId :: Text}
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON WindowId

instance Aeson.FromJSON WindowId

data Pane
  = Pane
      { paneWindowId :: WindowId,
        paneSessionId :: SessionId,
        paneSessionTitle :: Text,
        paneId :: PaneId,
        paneNumber :: Int,
        paneIsActive :: IsActive,
        paneCurrentCommand :: Text,
        paneCurrentPath :: Text,
        width :: Int,
        height :: Int,
        top :: Int,
        right :: Int,
        left :: Int,
        bottom :: Int
        -- TODO add
        -- is active
      }
  deriving (Generic)

instance Aeson.ToJSON Pane

instance Aeson.FromJSON Pane

newtype PaneId = PaneId {unPaneId :: Text}
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON PaneId

instance Aeson.FromJSON PaneId

data IsActive = Active | Inactive
  deriving (Eq, Generic)

instance Aeson.ToJSON IsActive

instance Aeson.FromJSON IsActive

listSessions :: MonadIO m => m [Session]
listSessions = do
  (unparsed, exitCode) <-
    Cmd.run
      "tmux"
      [ "list-sessions",
        "-F",
        T.intercalate
          ";"
          [ "#{session_id}",
            "#{session_name}",
            "#{session_attached}"
          ]
      ]
  pure $ case exitCode of
    ExitFailure _ -> []
    ExitSuccess ->
      case P.parseOnly (P.many' sessionParser) unparsed of
        Left _ -> []
        Right xs -> sortOn sessionTitle $ filter ((/=) "__TMUX_TUI_STARTUP" . sessionTitle) xs

listWindows :: MonadIO m => SessionId -> m [Window]
listWindows SessionId {unSessionId} = do
  (unparsed, exitCode) <-
    Cmd.run
      "tmux"
      [ "list-windows",
        "-F",
        T.intercalate
          ";"
          [ "#{window_panes}",
            "#{session_id}",
            "#{window_id}",
            "#{window_name}",
            "#{window_index}",
            "#{window_layout}",
            "#{pane_current_command}",
            "#{pane_current_path}",
            "#{session_name}",
            "#{window_active}"
          ],
        "-t",
        unSessionId
      ]
  pure $ case exitCode of
    ExitFailure _ -> []
    ExitSuccess ->
      case P.parseOnly (P.many' windowParser) unparsed of
        Left _ -> []
        Right xs ->
          sortOn number $ map fst $
            filter
              ( \(s@Window {sessionName, windowTitle, currentCommand}, paneCount) ->
                  not
                    ( sessionName == "__TMUX_TUI_STARTUP"
                        || (windowTitle == "tmux-tui" && paneCount == 1)
                    )
              )
              xs

data SessionOrWindowId = PanesForSessionId SessionId | PanesForWindowId SessionId WindowId

panesForWindow :: MonadIO m => SessionId -> WindowId -> m [Pane]
panesForWindow sessionId windowId = listPanes $ PanesForWindowId sessionId windowId

panesForSession :: MonadIO m => SessionId -> m [Pane]
panesForSession = listPanes . PanesForSessionId

listPanes :: MonadIO m => SessionOrWindowId -> m [Pane]
listPanes sessionOrWindowId = do
  (unparsed, exitCode) <-
    Cmd.run
      "tmux"
      $ mconcat
        [ ["list-panes"],
          case sessionOrWindowId of
            PanesForSessionId sessionId ->
              ["-s", "-t", unSessionId sessionId]
            PanesForWindowId sessionId windowId ->
              ["-t", unSessionId sessionId <> ":" <> unWindowId windowId],
          [ "-F",
            T.intercalate
              ";"
              [ "#{session_id}",
                "#{session_name}",
                "#{window_id}",
                "#{pane_id}",
                "#{pane_index}",
                "#{pane_active}",
                "#{pane_pid}",
                "#{pane_current_command}",
                "#{pane_current_path}",
                "#{pane_height}",
                "#{pane_width}",
                "#{pane_top}",
                "#{pane_bottom}",
                "#{pane_left}",
                "#{pane_right}"
              ]
          ]
        ]
  case exitCode of
    ExitFailure _ -> pure []
    ExitSuccess ->
      case P.parseOnly (P.many' paneParser) unparsed of
        Left _ -> pure []
        Right xs -> do
          editor <- T.pack <$> liftIO (getEnv "EDITOR")
          panes <-
            for
              xs
              ( \(x, pid) ->
                  if  | editor == paneCurrentCommand x -> pure (Just x)
                      | T.isInfixOf "tmux-tui" $ paneCurrentCommand x -> pure Nothing
                      | otherwise -> do
                        (out, exitCode) <- Cmd.run "ps" ["-ao", "ppid command"]
                        Just <$> case exitCode of
                          ExitFailure _ -> pure x
                          ExitSuccess -> do
                            case find (T.isInfixOf $ show $ pid) $ T.strip <$> T.lines out of
                              Just actual -> pure x {paneCurrentCommand = T.strip $ T.replace (show $ pid) "" actual}
                              Nothing -> pure x
              )
          pure $ M.catMaybes panes

sessionParser :: P.Parser (Session)
sessionParser = do
  sessionId <- SessionId <$> anyCharExceptSep
  separatorParser
  sessionTitle <- anyCharExceptSep
  separatorParser
  oneIfAttached <- P.decimal
  ignoreRest
  pure Session {sessionId, sessionTitle, isAttached = if oneIfAttached == 1 then Active else Inactive}

windowParser :: P.Parser (Window, Int)
windowParser = do
  paneCount <- P.decimal
  separatorParser
  windowSessionId <- SessionId <$> anyCharExceptSep
  separatorParser
  windowId <- WindowId <$> anyCharExceptSep
  separatorParser
  windowTitle <- anyCharExceptSep
  separatorParser
  number <- P.decimal
  separatorParser
  layout <- anyCharExceptSep
  separatorParser
  currentCommand <- anyCharExceptSep
  separatorParser
  currentPath <- anyCharExceptSep
  separatorParser
  sessionName <- anyCharExceptSep
  separatorParser
  oneIfActive <- P.decimal
  P.endOfLine
  pure
    ( Window
        { windowSessionId,
          windowId,
          number,
          windowTitle,
          currentCommand,
          currentPath,
          sessionName,
          isActive = if oneIfActive == 1 then Active else Inactive,
          layout
        },
      paneCount
    )

paneParser :: P.Parser (Pane, Int)
paneParser = do
  paneSessionId <- SessionId <$> anyCharExceptSep
  separatorParser
  paneSessionTitle <- anyCharExceptSep
  separatorParser
  paneWindowId <- WindowId <$> anyCharExceptSep
  separatorParser
  paneId <- PaneId <$> anyCharExceptSep
  separatorParser
  paneNumber <- P.decimal
  separatorParser
  oneIfActive <- P.decimal
  separatorParser
  panePid <- P.decimal
  separatorParser
  paneCurrentCommand <- anyCharExceptSep
  separatorParser
  paneCurrentPath <- anyCharExceptSep
  separatorParser
  height <- P.decimal
  separatorParser
  width <- P.decimal
  separatorParser
  top <- P.decimal
  separatorParser
  bottom <- P.decimal
  separatorParser
  left <- P.decimal
  separatorParser
  right <- P.decimal
  ignoreRest
  pure
    ( Pane
        { paneSessionId,
          paneSessionTitle,
          paneWindowId,
          paneId,
          paneNumber,
          paneCurrentPath,
          paneCurrentCommand,
          height,
          width,
          top,
          bottom,
          left,
          right,
          paneIsActive = if oneIfActive == 1 then Active else Inactive
        },
      panePid
    )

anyCharExceptSep :: P.Parser Text
anyCharExceptSep = T.pack <$> P.many' (P.notChar ';')

separatorParser :: P.Parser ()
separatorParser = const () <$> P.char ';'

ignoreRest :: P.Parser ()
ignoreRest = const () <$> P.manyTill P.anyChar P.endOfLine

data TmuxType = TmuxSession Session | TmuxWindow Window | TmuxPane Pane

suffix :: TmuxType -> Text
suffix (TmuxSession _) = "session"
suffix (TmuxWindow _) = "window"
suffix (TmuxPane _) = "pane"

target :: TmuxType -> Text
target (TmuxSession Session {sessionId}) = unSessionId sessionId
target (TmuxWindow window) = windowTarget window
target (TmuxPane pane) = paneTarget pane

windowTarget :: Window -> Text
windowTarget Window {windowSessionId, windowId} =
  unSessionId windowSessionId <> ":" <> unWindowId windowId

windowSession :: Window -> Text
windowSession Window {windowSessionId} =
  unSessionId windowSessionId <> ":"

paneTarget :: Pane -> Text
paneTarget Pane {paneSessionId, paneWindowId, paneId} =
  unSessionId paneSessionId <> ":" <> unWindowId paneWindowId <> "." <> unPaneId paneId

rename :: MonadIO m => Text -> TmuxType -> m ()
rename newTitle (TmuxPane _) = pure ()
rename newTitle tmuxType =
  Cmd.run_ "tmux" ["rename-" <> suffix tmuxType, "-t", target tmuxType, newTitle]

create :: MonadIO m => Text -> Maybe TmuxType -> m ()
create newTitle (Just (TmuxPane _)) = pure ()
create newTitle (Just (TmuxWindow window)) =
  Cmd.run_ "tmux" ["new-window", "-d", "-t", windowSession window, "-n", newTitle]
create newTitle _ = Cmd.run_ "tmux" ["new-session", "-d", "-s", newTitle]

switch :: MonadIO m => TmuxType -> m ()
switch tmuxType = do
  (_, exitCode) <- Cmd.run "tmux" ["switch-client", "-t", target tmuxType]
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> Cmd.run_ "tmux" ["attach", "-t", target tmuxType]

kill :: MonadIO m => TmuxType -> m ()
kill tmuxType = Cmd.run_ "tmux" ["kill-" <> suffix tmuxType, "-t", target tmuxType]

merge :: MonadIO m => Window -> Window -> m ()
merge a b = Cmd.run_ "tmux" $ ["join-pane", "-s", windowTarget a, "-t", windowTarget b]

swap :: MonadIO m => Window -> Window -> m ()
swap a b = Cmd.run_ "tmux" $ ["swap-window", "-s", windowTarget a, "-t", windowTarget b]

data Move = MovePane | MoveWindow

moveToString :: Move -> Text
moveToString MovePane = "pane"
moveToString MoveWindow = "window"

move :: MonadIO m => Move -> Text -> Text -> m ()
move m a b = Cmd.run_ "tmux" $ ["move-" <> moveToString m, "-d", "-s", a, "-t", b]

newtype Command = Command {unCommand :: Text}

runCommand :: MonadIO m => Text -> Command -> m ()
runCommand _ (Command "") = pure ()
runCommand target (Command command) =
  Cmd.run_
    "tmux"
    [ "send-keys",
      "-t",
      target,
      "C-z",
      "BSpace",
      command <> " || echo 'Could not run: " <> command <> "'",
      "Enter"
    ]
