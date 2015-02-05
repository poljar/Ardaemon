{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Maybe (fromMaybe)

import Control.Exception
import Control.Monad (forever)
import Control.Concurrent (forkIO)

import Network
import Network.JsonRpc.Server (call)

import System.Console.CmdArgs
import System.IO (Handle, hGetLine, hClose)

import qualified Data.ByteString.Lazy.Char8 as C

import Commands


data Options = Options {
                     port      :: Integer,
                     daemonize :: Bool
                     } deriving (Show, Data, Typeable)

options :: Options
options = Options {
                port      = 4040  &= help "Listnening port",
                daemonize = False &= help "Start daemon in background"
                }
                &= summary "Arduino control daemon 0.1"


main :: IO ()
main = withSocketsDo $ do
    Options {..} <- cmdArgs options

    sock <- listenOn $ PortNumber $ fromInteger port
    mainLoop sock


mainLoop :: Socket -> IO ()
mainLoop sock = do
    (hdl, _, _) <- accept sock
    _ <- forkIO $ runConn hdl
    mainLoop sock


handleMsg :: C.ByteString -> IO C.ByteString
handleMsg msg = do
            response <- call methods msg
            return (fromMaybe "" response)


runConn :: Handle -> IO ()
runConn hdl = do

    handle (\(SomeException _) -> return ()) $ forever $ do
        contents <- fmap C.pack (hGetLine hdl)
        C.putStrLn contents
        response <- mapM handleMsg $ C.lines contents
        mapM_ (C.hPutStrLn hdl) response

    hClose hdl
