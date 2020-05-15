module Tmux.Stored
  ( Session (..),
    instantiate,
    persist,
    delete,
    list,
  )
where

import qualified Cmd
import qualified Data.Aeson as Aeson
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Time as Time
import GHC.Generics (Generic)
import Protolude hiding (list)
import qualified System.Directory as Dir
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import qualified Tmux

data Session
  = Session
      { session :: Tmux.Session,
        windows :: [Tmux.Window],
        panes :: [Tmux.Pane],
        lastModifiedAt :: Time.UTCTime
      }
  deriving (Generic)

instance Aeson.ToJSON Session

instance Aeson.FromJSON Session

instantiate :: MonadIO m => Session -> Text -> m ()
instantiate Session {session, windows, panes} sessionTitle = do
  for_ (zip [0 ..] windows) $ \(number, w@Tmux.Window {Tmux.windowTitle}) -> do
    let prevWindow = sessionTitle <> ":" <> show (number - 1)
    case filter ((==) (Tmux.windowId w) . Tmux.paneWindowId) panes of
      [] -> do
        windowOrSession
          WindowOrSession {number, sessionTitle, windowTitle, prevWindow, path = Tmux.currentPath w}
        Tmux.runCommand sessionTitle (Tmux.Command $ Tmux.currentCommand w)
      p : ps -> do
        let currWindow = sessionTitle <> ":" <> show number
        windowOrSession
          WindowOrSession {number, sessionTitle, windowTitle, prevWindow, path = Tmux.paneCurrentPath p}
        Tmux.runCommand (currWindow <> ".0") (Tmux.Command $ Tmux.paneCurrentCommand p)
        Cmd.run_ "tmux" ["select-layout", "-t", currWindow, Tmux.layout w]
        for_ (zip [1 ..] ps) $ \(pidx, x) -> do
          Cmd.run_ "tmux" ["split-window", "-t", currWindow, "-c", Tmux.paneCurrentPath x]
          Tmux.runCommand (currWindow <> "." <> show pidx) (Tmux.Command $ Tmux.paneCurrentCommand x)
          Cmd.run_ "tmux" ["select-layout", "-t", currWindow, Tmux.layout w]

data WindowOrSession
  = WindowOrSession
      { number :: Int,
        sessionTitle :: Text,
        windowTitle :: Text,
        prevWindow :: Text,
        path :: Text
      }

windowOrSession :: MonadIO m => WindowOrSession -> m ()
windowOrSession WindowOrSession {number, sessionTitle, windowTitle, prevWindow, path} =
  Cmd.run_ "tmux" $
    if number == 0
      then ["new-session", "-d", "-s", sessionTitle, "-n", windowTitle, "-c", path]
      else ["new-window", "-d", "-a", "-t", prevWindow, "-n", windowTitle, "-c", path]

persist :: MonadIO m => Tmux.Session -> m ()
persist session = liftIO $ do
  lastModifiedAt <- Time.getCurrentTime
  windows <- Tmux.listWindows $ Tmux.sessionId session
  panes <- Tmux.panesForSession $ Tmux.sessionId session
  sessionsDir <- getSessionsDir
  Aeson.encodeFile (sessionsDir </> T.unpack (Tmux.sessionTitle session)) $
    Session
      { lastModifiedAt,
        session,
        windows,
        panes
      }

delete :: MonadIO m => Text -> m ()
delete id = liftIO $ do
  sessionsDir <- getSessionsDir
  Dir.removeFile (sessionsDir </> T.unpack id)

list :: MonadIO m => m [Session]
list = liftIO $ do
  sessionsDir <- getSessionsDir
  files <- Dir.listDirectory sessionsDir
  sortOn (Tmux.sessionTitle . session) . M.catMaybes <$> traverse (Aeson.decodeFileStrict' . (</>) sessionsDir) files

getSessionsDir :: IO FilePath
getSessionsDir = do
  home <- Dir.getHomeDirectory
  let sessionsDir = (home </> ".config" </> "tmux-tui" </> "stored-sessions")
  Dir.createDirectoryIfMissing True sessionsDir
  pure sessionsDir
