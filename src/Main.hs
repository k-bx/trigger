module Main where

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exc
import qualified Control.Exception.Safe as ExcSafe
import Control.Monad (forM_, forever)
import System.Directory (canonicalizePath)
import qualified System.Environment as SE
import qualified System.FSNotify as FSNotify
import qualified System.FilePath as FilePath
import System.IO
import qualified System.Posix.Signals as Signals
import qualified System.Posix.Types as PosixTypes
import qualified System.Process as Proc

data Signal =
  FileChanged

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  args <- SE.getArgs
  case args of
    [] -> putStrLn "> Please put your command as an argument"
    (x:xs) -> do
      putStrLn $ "> Running a command: " <> x
      putStrLn $ "> Arguments: " <> show xs
      cleanupPids <- MVar.newMVar []
      chan <- Chan.newChan
      a1 <- Async.async (launcher chan cleanupPids x xs)
      a2 <- Async.async (listener chan x)
      flip Exc.finally (cleanup cleanupPids) $ do
        _ <- (Async.waitAny [a1, a2])
        forever (Concurrent.threadDelay delay)

delay :: Int
delay = 1000000

logAndRestart :: IO () -> IO ()
logAndRestart f =
  (f `ExcSafe.catchAny` \e -> putStrLn $ "Got exception: " <> show e)
  -- (f `Exc.catch` \e -> putStrLn $ "Got exception: " <> show (e::Exc.SomeException))

launcher ::
     Chan.Chan Signal -> MVar.MVar [Proc.Pid] -> FilePath -> [String] -> IO ()
launcher chan cleanupPids x xs = logAndRestart $ do
  pHandle <- Proc.spawnProcess x xs
  mPid <- Proc.getPid pHandle
  _ <- MVar.swapMVar cleanupPids (maybe [] (\y -> [y]) mPid)
  sig <- Chan.readChan chan
  case sig of
    FileChanged -> do
      case mPid of
        Nothing -> pure ()
        Just pid -> Signals.signalProcess Signals.killProcess pid
      _ <- MVar.swapMVar cleanupPids []
      launcher chan cleanupPids x xs

cleanup :: Foldable t => MVar.MVar (t PosixTypes.ProcessID) -> IO ()
cleanup pidsMvar = do
  putStrLn "> Cleanup launched"
  pids <- MVar.readMVar pidsMvar
  putStrLn $ "> length(pids): " <> show (length pids)
  forM_ pids $ \pid -> do Signals.signalProcess Signals.killProcess pid

listener :: Chan.Chan Signal -> FilePath -> IO ()
listener chan fp' = do
  fp <- canonicalizePath fp'
  putStrLn $ "> Listener is listening to: " <> fp
  putStrLn $ "> Directory is: " <> FilePath.takeDirectory fp
  _ <-
    FSNotify.withManager $ \mgr -> do
      let predicate ev =
            case ev of
              FSNotify.Modified mpath _t _bool -> mpath == fp
              FSNotify.Added mpath _t _bool -> mpath == fp
              FSNotify.Unknown mpath _t _bool -> mpath == fp
              _ -> False
      _ <-
        FSNotify.watchDir mgr (FilePath.takeDirectory fp) predicate $ \ev -> do
          putStrLn $ "> FSNotify signal: " <> show ev
          Chan.writeChan chan FileChanged
      forever (Concurrent.threadDelay delay)
  forever (Concurrent.threadDelay delay)
