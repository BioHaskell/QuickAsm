-- | Variant of a System.Process.readProcessWithExitCode that 
-- also changes working directory.
{-# LANGUAGE CPP, InterruptibleFFI, Trustworthy, ForeignFunctionInterface #-}
module Util.Process(readProcessWithExitCodeAndWorkingDir) where

import Prelude hiding (mapM)

import System.Process.Internals
import System.Process(terminateProcess, waitForProcess, createProcess, proc)

import Control.Exception (SomeException, mask, try, onException, throwIO)
import Control.DeepSeq (rnf)
import System.IO.Error (mkIOError, ioeSetErrorString)
#if !defined(mingw32_HOST_OS)
import System.Posix.Types
#endif
import qualified Control.Exception as C
import Control.Concurrent
import Control.Monad
import Foreign
import Foreign.C
import System.IO
import Data.Maybe
import System.Exit      ( ExitCode(..) )

import GHC.IO.Exception ( ioException, IOErrorType(..), IOException(..) )
#if defined(mingw32_HOST_OS)
import System.Win32.Process (getProcessId)
import System.Win32.Console (generateConsoleCtrlEvent, cTRL_BREAK_EVENT)
#else
import System.Posix.Signals
#endif

forkWait :: IO a -> IO (IO a)
forkWait a = do
  res <- newEmptyMVar
  _ <- mask $ \restore -> forkIO $ try (restore a) >>= putMVar res
  return (takeMVar res >>= either (\ex -> throwIO (ex :: SomeException)) return)

readProcessWithExitCodeAndWorkingDir cmd args input workingDir =
    mask $ \restore -> do
      (Just inh, Just outh, Just errh, pid) <- createProcess (proc cmd args)
                                                   { std_in  = CreatePipe,
                                                     std_out = CreatePipe,
                                                     std_err = CreatePipe,
                                                     cwd     = workingDir }
      flip onException
        (do hClose inh; hClose outh; hClose errh;
            terminateProcess pid; waitForProcess pid) $ restore $ do
        -- fork off a thread to start consuming stdout
        out <- hGetContents outh
        waitOut <- forkWait $ C.evaluate $ rnf out

        -- fork off a thread to start consuming stderr
        err <- hGetContents errh
        waitErr <- forkWait $ C.evaluate $ rnf err

        -- now write and flush any input
        let writeInput = do
              unless (null input) $ do
                hPutStr inh input
                hFlush inh
              hClose inh

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 611
        C.catch writeInput $ \e -> case e of
          IOError { ioe_type = ResourceVanished
                  , ioe_errno = Just ioe }
            | Errno ioe == ePIPE -> return ()
          _ -> throwIO e
#else
        writeInput
#endif

        -- wait on the output
        waitOut
        waitErr

        hClose outh
        hClose errh

        -- wait on the process
        ex <- waitForProcess pid

        return (ex, out, err)


