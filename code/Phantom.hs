{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

data Part = Part {
    -- name of the <input> tag this belongs to
      name        :: String
    -- filename of file we're uploading
    , fileName    :: Maybe FilePath
    -- type of file
    , contentType :: Maybe ContentType
    -- file contents
    , body        :: String
    } deriving (Show)

type Param = (String, String)

type ContentType = String

data Payload a = NoPayload
               | Raw ContentType String
             | Params [Param]
             | FormData [Part]
               deriving (Show)

instance Monoid Payload where
    mempty = NoPayload

    mappend NoPayload b = b
    mappend a NoPayload = a

    mappend (Params a) (Params b) = Params (a++b)
