module Cmd (run, run_, silently_) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Protolude
import qualified System.Process as SP

run :: MonadIO m => Text -> [Text] -> m (Text, ExitCode)
run c a = liftIO $ do
  (_, Just hout, _, ph) <- SP.createProcess (SP.proc (T.unpack c) (T.unpack <$> filter ((/=) "") a)) {SP.std_out = SP.CreatePipe}
  exitCode <- SP.waitForProcess ph
  out <- TIO.hGetContents hout
  pure (out, exitCode)

run_ :: MonadIO m => Text -> [Text] -> m ()
run_ c a = liftIO $ do
  ph <- SP.spawnProcess (T.unpack c) (T.unpack <$> filter ((/=) "") a)
  _ <- SP.waitForProcess ph
  pure ()

silently_ :: MonadIO m => Text -> [Text] -> m ()
silently_ c a =
  liftIO $ do
    _ <- SP.createProcess (SP.proc (T.unpack c) (T.unpack <$> filter ((/=) "") a)) {SP.std_err = SP.CreatePipe, SP.std_out = SP.CreatePipe}
    pure ()
