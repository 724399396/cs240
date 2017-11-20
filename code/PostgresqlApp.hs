{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics

data Article = Article {
  articleId          :: DBKey
  , articleTitle     :: Text
  , articleBody      :: Text
  , articleShortName :: Text
  }

instance Model Article

data DBKey = DBKey !Int64 | NullKey

data ModelInfo a = ModelInfo {
  modelTable           :: ByteString
  , modelColumns       :: [ByteString]
  , modelPrimaryColumn :: Int
  , modelGetPrimaryKey :: a -> DBKey
  }
