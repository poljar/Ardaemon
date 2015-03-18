{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Network.JsonRpc.Server
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad.Reader

data ProcCom m = ProcCom {processVar :: MVar m, referenceChan :: Chan m}

type Server = ReaderT (ProcCom Int) IO

methods :: Methods Server
methods = toMethods [add, seven, getLevel]

add :: Method Server
add = toMethod "add" f (Required "x" :+: Required "y" :+: ())
    where f :: Double -> Double -> RpcResult Server Double
          f x y = return (x + y)

seven :: Method Server
seven = toMethod "seven" f ()
        where
        f :: RpcResult Server Double
        f = return x
        x = 7 :: Double

getLevel :: Method Server
getLevel = toMethod "get-level" f ()
    where f :: RpcResult Server Int
          f = do
                pv <- liftM processVar ask
                x <- liftIO $ readMVar pv
                return x
