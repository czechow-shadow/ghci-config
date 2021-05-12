{-# LANGUAGE NoImplicitPrelude #-}
module PC where

import Prelude

import System.Process
import System.Posix.IO
import Control.Exception
import Control.Monad.IO.Class
import Text.Pretty.Simple (pPrint)


less :: IO a -> IO a
less action = do
  stdout_copy <- dup stdOutput

  (Just pipe_handle, Nothing, Nothing, pid)
    <- createProcess (proc "less" []) { std_in = CreatePipe }
  closeFd stdOutput
  pipe_fd <- handleToFd pipe_handle
  _ <- dupTo pipe_fd stdOutput
  closeFd pipe_fd

  action `finally` closeFd stdOutput
         `finally` waitForProcess pid
         `finally` do _ <- dupTo stdout_copy stdOutput
                      closeFd stdout_copy

lessGhci :: String -> IO String
lessGhci args = pure $ "PC.less (" <> args <> ")"

pp :: (MonadIO m, Show a) => a -> m ()
pp = pPrint
