module Arduino where

import Control.Concurrent.MVar
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (forever)

import System.Hardware.Arduino
import System.Hardware.Serialport

import Data.Word (Word8)
import qualified Data.ByteString as B

import Commands (PVType)


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

shutDownArduino :: FilePath -> IO ()
shutDownArduino arduinoPort = do
        port <- openSerial arduinoPort defaultSerialSettings { commSpeed = CS57600 }
        _ <- send port $ B.pack [0xFF :: Word8]
        closeSerial port
