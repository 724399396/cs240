module Main where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp   (run)

app :: Application
app _ respond = respond $ responseLBS status200 [] $ L8.pack "Hello, World"

main :: IO ()
main = do
  run 3000 app
