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
            let integral_term     = integral + ki * error / fromIntegral sampleTime

            let piOut = proportional_term + integral_term

            let new_integral = if piOut > maxOut ||
                                   piOut < minOut then
                                    integral
                                else
                                    integral_term

            let output = clampAndScaleOutput $ proportional_term + new_integral

            liftIO $ putMVar integralMVar new_integral

            liftIO $ putStrLn ("Ref: " ++ show reference ++ " sensorValue: "
                               ++ show sensorValue ++ " height: " ++ show fillHeight)

            liftIO $ putStrLn ("Error: " ++ show error ++ " integral: " ++ show new_integral)

            analogWrite pwm_pin output
            delay sampleTime
        where
                kp = 4
                ki = 1.5
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
                            where sensor_constant | value < 80  = 0.0533
                                                  | value < 150 = 0.0431
                                                  | value < 200 = 0.0375
                                                  | value < 220 = 0.0341
                                                  | value < 250 = 0.0327
                                                  | value < 300 = 0.0312
                                                  | value < 350 = 0.0292
                                                  | value < 400 = 0.0289
                                                  | value < 450 = 0.0286
                                                  | value < 500 = 0.0281
                                                  | value < 550 = 0.0270
                                                  | value < 600 = 0.0259
                                                  | value < 650 = 0.0262
                                                  | value < 700 = 0.0254
                                                  | otherwise   = 0.0322


shutDownArduino :: FilePath -> IO ()
shutDownArduino arduinoPort = do
        port <- openSerial arduinoPort defaultSerialSettings { commSpeed = CS57600 }
        _ <- send port $ B.pack [0xFF :: Word8]
        closeSerial port
