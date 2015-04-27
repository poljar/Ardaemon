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

import System.Hardware.Arduino
import System.Hardware.Serialport
import qualified Data.ByteString as B
import Data.Word (Word8)


import Commands


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
        port <- openSerial arduinoPort defaultSerialSettings { commSpeed = CS57600 }
        _ <- send port $ B.pack [0xFF :: Word8]
        closeSerial port

    putStrLn "Shutting daemon down."


controlLoop :: FilePath -> MVar PVType -> MVar PVType -> IO ()
controlLoop arduinoPath refMVar pvMVar = withArduino False arduinoPath $ do
        liftIO $ putStrLn "Daemon startup finished"
        let sensor      = analog 0
        let pwm_pin     = digital 3
        let chip_enable = digital 2

        setPinMode sensor ANALOG
        setPinMode pwm_pin PWM
        setPinMode chip_enable OUTPUT
        digitalWrite chip_enable True

        integralMVar <- liftIO $ newMVar 0

        forever $ do
            integral  <- liftIO $ takeMVar integralMVar
            reference <- liftIO $ readMVar refMVar

            sensorValue <- analogRead sensor
            let fillHeight = sensorValueFunc sensorValue
            _ <- liftIO $ swapMVar pvMVar fillHeight

            let error = reference - fillHeight

            let proportional_term = kp * error

            let newIntegral = if proportional_term < maxOut &&
                                 proportional_term > minOut then
                                  (integral + error / fromIntegral sampleTime) * ki
                              else
                                  integral

            let output = clampAndScaleOutput $ kp * error + newIntegral

            liftIO $ putMVar integralMVar newIntegral

            liftIO $ putStrLn ("Ref: " ++ show reference ++ " sensorValue: "
                               ++ show sensorValue ++ " height: " ++ show fillHeight)

            liftIO $ putStrLn ("Error: " ++ show error ++ " integral: " ++ show newIntegral)

            analogWrite pwm_pin output
            delay sampleTime
        where
                kp = 4
                ki = 1
                minOut = 3
                maxOut = 12
                sampleTime = 100
                clampAndScaleOutput out
                    | out <  1.5      = 0
                    | out <  minOut   = 64
                    | out >= maxOut   = 255
                    | otherwise       = round $ (out / 12) * 255


sensorValueFunc :: Int -> Double
sensorValueFunc value = fromIntegral value * sensor_constant
                            where sensor_constant = 0.0296


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
