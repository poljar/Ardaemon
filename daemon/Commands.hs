{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Network.JsonRpc.Server
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad.Reader
import Control.Monad.Error (throwError)

type PVType = Double

data ProcCom m = ProcCom {processVar :: MVar m, referenceChan :: Chan m}

type Server = ReaderT (ProcCom PVType) IO

methods :: Methods Server
methods = toMethods [add, seven, getLevel, setReference]

add :: Method Server
add = toMethod "add" f (Required "x" :+: Required "y" :+: ())
    where f :: Double -> Double -> RpcResult Server Double
          f x y = return (x + y)

seven :: Method Server
seven = toMethod "seven" f ()
    where f :: RpcResult Server Double
          f = return x
          x = 7 :: Double

getLevel :: Method Server
getLevel = toMethod "get-level" f ()
    where f :: RpcResult Server PVType
          f = do
                pv <- asks processVar
                liftIO $ readMVar pv

referenceIsValid :: PVType -> Bool
referenceIsValid ref
    | ref > 20   = False
    | ref < 0    = False
    | otherwise  = True

referenceIsNotValid :: PVType -> Bool
referenceIsNotValid ref = not $ referenceIsValid ref

setReference :: Method Server
setReference = toMethod "set-reference" f (Required "reference" :+: ())
    where f :: PVType -> RpcResult Server ()
          f ref = do
                when (referenceIsNotValid ref) $ throwError invalidReference

                refChan <- asks referenceChan
                liftIO $ writeChan refChan ref
          invalidReference = rpcError (-32000) "Reference out of range (0-20cm)!"
