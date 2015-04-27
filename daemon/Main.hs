{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Maybe (fromMaybe)

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad.Reader (forever, runReaderT)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.Trans (liftIO)

import Network
import Network.JsonRpc.Server (call)

import System.Exit
import System.Console.CmdArgs
import System.Directory (doesFileExist)
import System.IO (Handle, hGetLine, hIsEOF, hClose)

import qualified Data.ByteString.Lazy.Char8 as C


import Commands
import Arduino


data Options = Options {
     port        :: Integer,
     arduinoPort :: FilePath,
     simulate    :: Bool
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

    if pathValid || simulate then
        startDaemon port arduinoPort simulate
    else do
        putStrLn ("No such file: " ++ show arduinoPort)
        exitFailure


startDaemon :: Integer -> FilePath -> Bool -> IO ()
startDaemon port arduinoPort simulate = do
    sock <- listenOn $ PortNumber $ fromInteger port

    pv <- newMVar 0
    referenceChan <- newChan
    let com = ProcCom pv referenceChan

    _ <- forkIO $ mainLoop sock com

    controllerBroker arduinoPort simulate com


mainLoop :: Socket -> ProcCom PVType -> IO ()
mainLoop sock com = do
    (hdl, _, _) <- accept sock
    _ <- forkIO $ runConn hdl com

    mainLoop sock com


controllerBroker :: FilePath -> Bool -> ProcCom PVType -> IO ()
controllerBroker arduinoPort simulate (ProcCom pvMVar refChan) = do
    refMVar <- newMVar 0

    _ <- forkIO $ forever $ do
            ref <- readChan refChan
            swapMVar refMVar ref

    if simulate then
        simulatorLoop refMVar pvMVar
    else do
        controlLoop arduinoPort refMVar pvMVar
        shutDownArduino arduinoPort

    putStrLn "Shutting daemon down."


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
        isEof <- hIsEOF hdl

        if isEof then
            hClose hdl
        else do
            contents <- fmap C.pack (hGetLine hdl)
--            C.putStrLn contents
            response <- mapM (handleMsg com) $ C.lines contents
            mapM_ (C.hPutStrLn hdl) response
            runConn hdl com
