{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Files.Conduit (StreamFileHandler (..), Ext (..)) where

import Conduit
  ( ConduitT,
    awaitForever,
    filterC,
    mapC,
    sourceDirectoryDeep,
    sourceFile,
    (.|),
  )
import Control.Monad.Trans.Resource (MonadResource)
import Data.ByteString.Char8 (unpack)
import Data.Char (toUpper)
import System.FilePath (takeExtension)

data Ext = Txt

instance Show Ext where
  show Txt = ".txt"

class MonadResource m => StreamFileHandler m where
  read :: FilePath -> Ext -> ConduitT () [String] m ()

instance MonadResource m => StreamFileHandler m where
  read dir' ext' =
    sourceDirectoryDeep False dir'
      .| filterC (\a -> takeExtension a == show ext')
      .| awaitForever transform
    where
      transform :: MonadResource m => String -> ConduitT i [String] m ()
      transform fp =
        sourceFile fp
          .| mapC unpack
          .| mapC toList

toList :: String -> [String]
toList = lines
