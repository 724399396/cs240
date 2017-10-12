{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

import Data.Monoid
import Data.IORef

newtype FirstRight a b = FirstRight {
    getFirstRight :: Either a b
  }

instance Monoid (FirstRight String a) where
  mempty = FirstRight $ Left ""
  mappend r@(FirstRight (Right _)) _ = r
  mappend _ o = o

data Part = Part {
    -- name of the <input> tag this belongs to
    name        :: String
    -- filename of file we're uplogin
  , fileName    :: Maybe FilePath
    -- type of file
  , contentType :: Maybe ContentType
    -- file content
  , body        :: String
  } deriving (Show)

type Param = (String, String)
type ContentType = String
data Payload a = NoPayload
               | Raw ContentType String
               | Params [Param]
               | FormData [Part]
               deriving (Show)

param :: String -> String -> Payload [Param]
param name value = Params [(name, value)]

filePart :: String -> FilePath -> IO (Payload [Part])
filePart name path = do
  body <- readFile path
  return $ FormData [Part name (Just path) Nothing body]

instance Monoid (Payload [Param]) where
  mempty = NoPayload
  mappend = addParams

addParams :: Payload [Param] -> Payload [Param] -> Payload [Param]
addParams a NoPayload = a
addParams NoPayload b = b
addParams (Params a) (Params b) = Params (a ++ b)

newtype Ref t a = Ref (IORef a)

data ReadOnly
data ReadWrite

newRef :: a -> IO (Ref ReadWrite a)
newRef a = Ref <$> newIORef a

readRef :: Ref t a -> IO a
readRef (Ref ref) = readIORef ref

writeRef :: Ref ReadWrite a -> a -> IO ()
writeRef (Ref ref) v = writeIORef ref v

readOnly :: Ref t a -> Ref ReadOnly a
readOnly (Ref ref) = Ref ref
