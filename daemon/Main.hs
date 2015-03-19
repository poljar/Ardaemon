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
     arduinoPort :: FilePath
     } deriving (Show, Data, Typeable)

options :: Options
options = Options {
    port        = 4040            &= help "Listnening port",
    arduinoPort = "/dev/ttyACM0"  &= help "Path to the arduino port"
    }
    &= summary "Arduino control daemon 0.1"


main :: IO ()
main = withSocketsDo $ do
    Options {..} <- cmdArgs options

    pathValid <- doesFileExist arduinoPort

    case pathValid of
        True  -> startDaemon port arduinoPort
        False -> do
                putStrLn ("No such file: " ++ show(arduinoPort))
                exitFailure


startDaemon :: Integer -> FilePath -> IO ()
startDaemon port _ = do
    sock <- listenOn $ PortNumber $ fromInteger port

    pv <- newMVar 20
    referenceChan <- newChan
    let com = ProcCom pv referenceChan

    _ <- forkIO $ controllerBroker com

    mainLoop sock com


mainLoop :: Socket -> ProcCom Int -> IO ()
mainLoop sock com = do
    (hdl, _, _) <- accept sock
    _ <- forkIO $ runConn hdl com

    mainLoop sock com


controllerBroker :: ProcCom Int -> IO ()
controllerBroker (ProcCom pvMVar refChan) = do
    refMVar <- newMVar 0
    _ <- forkIO $ controlLoop refMVar pvMVar

    forever $ do
        ref <- readChan refChan
        swapMVar refMVar ref


controlLoop :: MVar Int -> MVar Int -> IO ()
controlLoop refMVar pvMVar = forever $ do
    -- connect to the arduino or exitFailure
    ref <- readMVar refMVar
    -- measure the PV here
    _ <- swapMVar pvMVar 15
    -- implement a controler here
    print ref
    threadDelay 1000000


handleMsg :: ProcCom Int -> C.ByteString -> IO C.ByteString
handleMsg com msg = do
    response <- runReaderT (call methods msg) com
    return (fromMaybe "" response)


runConn :: Handle -> ProcCom Int -> IO ()
runConn hdl com = do

    handle (\(SomeException _) -> return ()) $ forever $ do
        contents <- fmap C.pack (hGetLine hdl)
        C.putStrLn contents
        response <- mapM (handleMsg com) $ C.lines contents
        mapM_ (C.hPutStrLn hdl) response

    hClose hdl
