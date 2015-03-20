{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Maybe (fromMaybe)

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad.Reader
import Control.Concurrent (forkIO, threadDelay)

import Network
import Network.JsonRpc.Server (call)

import System.Exit
import System.Console.CmdArgs
import System.Directory (doesFileExist)
import System.IO (Handle, hGetLine, hClose)

import qualified Data.ByteString.Lazy.Char8 as C

import Commands


data Options = Options {
     port        :: Integer,
     arduinoPort :: FilePath,
     simulate :: Bool
     } deriving (Show, Data, Typeable)

options :: Options
options = Options {
    port        = 4040            &= help "Listnening port",
    arduinoPort = "/dev/ttyACM0"  &= help "Path to the arduino port",
    simulate    = False           &= help "Simulate controller"
    }
    &= summary "Arduino control daemon 0.1"


main :: IO ()
main = withSocketsDo $ do
    Options {..} <- cmdArgs options

    pathValid <- doesFileExist arduinoPort

    case pathValid of
        True  -> startDaemon port arduinoPort simulate
        False -> case simulate of
            True  -> startDaemon port arduinoPort simulate
            False -> do
                putStrLn ("No such file: " ++ show(arduinoPort))
                exitFailure


startDaemon :: Integer -> FilePath -> Bool -> IO ()
startDaemon port arduinoPort simulate = do
    sock <- listenOn $ PortNumber $ fromInteger port

    pv <- newMVar 0
    referenceChan <- newChan
    let com = ProcCom pv referenceChan

    _ <- forkIO $ controllerBroker arduinoPort simulate com

    mainLoop sock com


mainLoop :: Socket -> ProcCom PVType -> IO ()
mainLoop sock com = do
    (hdl, _, _) <- accept sock
    _ <- forkIO $ runConn hdl com

    mainLoop sock com


controllerBroker :: FilePath -> Bool -> ProcCom PVType -> IO ()
controllerBroker arduinoPort simulate (ProcCom pvMVar refChan) = do
    refMVar <- newMVar 0

    _ <- case simulate of
        True  -> forkIO $ simulatorLoop refMVar pvMVar
        False -> forkIO $ controlLoop arduinoPort refMVar pvMVar

    forever $ do
        ref <- readChan refChan
        swapMVar refMVar ref


controlLoop :: FilePath -> MVar PVType -> MVar PVType -> IO ()
controlLoop _ refMVar pvMVar = forever $ do
    -- connect to the arduino or exitFailure
    ref <- readMVar refMVar
    -- measure the PV here
    _ <- swapMVar pvMVar ref
    -- implement a controler here
    threadDelay 1000000


simulatorLoop :: MVar PVType -> MVar PVType -> IO ()
simulatorLoop refMVar pvMVar = forever $ do
    ref <- readMVar refMVar
    curPV <- readMVar pvMVar

    let newPV = curPV + (ref - curPV) * 0.1

    _ <- swapMVar pvMVar newPV

    threadDelay 1000000


handleMsg :: ProcCom PVType -> C.ByteString -> IO C.ByteString
handleMsg com msg = do
    response <- runReaderT (call methods msg) com
    return (fromMaybe "" response)


runConn :: Handle -> ProcCom PVType -> IO ()
runConn hdl com = do

    handle (\(SomeException _) -> return ()) $ forever $ do
        contents <- fmap C.pack (hGetLine hdl)
        C.putStrLn contents
        response <- mapM (handleMsg com) $ C.lines contents
        mapM_ (C.hPutStrLn hdl) response

    hClose hdl
