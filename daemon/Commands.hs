{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Network.JsonRpc.Server
import Control.Monad.IO.Class (liftIO)

type Server = IO

methods :: Methods Server
methods = toMethods [add, seven]

add :: Method Server
add = toMethod "add" f (Required "x" :+: Required "y" :+: ())
    where f :: Double -> Double -> RpcResult Server Double
          f x y = liftIO $ return (x + y)

seven :: Method Server
seven = toMethod "seven" f ()
        where
        f = return x
        x = 7 :: Double
