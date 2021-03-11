module Main where

import qualified Basic.Conduit as B
import Conduit (runConduitRes, sinkList, (.|))
import qualified Files.Conduit as F
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

main :: IO ()
main = do
  _ <- B.run
  directory <- (</> "files/in") <$> getCurrentDirectory
  l <- runConduitRes $ F.read directory F.Txt .| sinkList
  print l
