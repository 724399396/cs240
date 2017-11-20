{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Simple
import Network.Wai.Handler.Warp
import System.Posix.Env

app :: (Application -> IO ()) -> IO ()
app runner = runner $ do
  controllerApp () $ do
    respond $ okHtml "Hello World"

main :: IO ()
main = do
  port <- read `fmap` getEnvDefault "PORT" "3000"
  app (run port)
