{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Maybe (fromMaybe)

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad.Reader (forever, runReaderT)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.Trans (liftIO)

import Network.Socket
import Network.JsonRpc.Server (call)

import System.Exit
import System.Log.Logger
import System.Console.CmdArgs
import System.Directory (doesFileExist)
import System.IO (Handle, hGetLine, hIsEOF, hClose, IOMode(..))

import qualified Data.ByteString.Lazy.Char8 as C


import Commands
import Arduino


data Options = Options {
     port            :: Integer,
     arduinoPort     :: FilePath,
     simulate        :: Bool,
     debugRegulator  :: Bool,
     verbose         :: Bool
     } deriving (Show, Data, Typeable)

options :: Options
options = Options {
    port           = 4040            &= help "Listnening port",
    arduinoPort    = "/dev/ttyACM0"  &= help "Path to the arduino port",
    simulate       = False           &= help "Simulate controller",
    debugRegulator = False           &= help "Enable debugging info for the regulator",
    verbose        = False           &= help "Output more"
    }
    &= summary "Arduino control daemon 0.1"


main :: IO ()
main = do
    Options {..} <- cmdArgs options

    pathValid <- doesFileExist arduinoPort

    if debugRegulator then
        updateGlobalLogger "Regulator" (setLevel DEBUG)
    else
        updateGlobalLogger "Regulator" (setLevel NOTICE)

    if verbose then
        updateGlobalLogger rootLoggerName (setLevel DEBUG)
    else
        updateGlobalLogger rootLoggerName (setLevel NOTICE)

    if pathValid || simulate then
        startDaemon port arduinoPort simulate
    else do
        errorM rootLoggerName $ "No such file: " ++ show arduinoPort
        exitFailure


startDaemon :: Integer -> FilePath -> Bool -> IO ()
startDaemon port arduinoPort simulate = do
    sock <- socket AF_INET Stream defaultProtocol

    bindSocket sock $ SockAddrInet (fromInteger port) iNADDR_ANY
    listen sock 50

    pv <- newMVar 0
    referenceChan <- newChan
    let com = ProcCom pv referenceChan

    _ <- forkIO $ mainLoop sock com

    controllerBroker arduinoPort simulate com


mainLoop :: Socket -> ProcCom PVType -> IO ()
mainLoop sock com = do
    (s, host) <- accept sock
    let hostinfo = show host
    noticeM rootLoggerName $ "Connected:    " ++ hostinfo
    hdl <- socketToHandle s ReadWriteMode
    _ <- forkIO $ runConn hdl com hostinfo

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

    noticeM rootLoggerName "Shutting daemon down."


simulatorLoop :: MVar PVType -> MVar PVType -> IO ()
simulatorLoop refMVar pvMVar = do
    noticeM rootLoggerName "Daemon startup finished"

    forever $ do
        ref <- readMVar refMVar
        curPV <- readMVar pvMVar

        let newPV = curPV + (ref - curPV) * 0.1

        _ <- swapMVar pvMVar newPV

        threadDelay 1000000


handleMsg :: ProcCom PVType -> C.ByteString -> IO C.ByteString
handleMsg com msg = do
    response <- runReaderT (call methods msg) com
    return (fromMaybe "" response)


runConn :: Handle -> ProcCom PVType -> String -> IO ()
runConn hdl com hostinfo = do
    isEof <- hIsEOF hdl

    if isEof then do
        hClose hdl
        noticeM rootLoggerName $ "Disconnected: " ++ hostinfo
    else do
        contents <- hGetLine hdl
        debugM rootLoggerName $ "RPC request : " ++ contents

        response <- handleMsg com $ C.pack contents
        debugM rootLoggerName $ "RPC response: " ++ C.unpack response

        C.hPutStrLn hdl response
        runConn hdl com hostinfo
