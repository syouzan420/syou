{-# LANGUAGE OverloadedStrings #-}
module Convert where

import qualified Data.Text as T

import File (fileRead, fileWrite)

conv :: IO ()
conv = do
  lns <- T.lines . repText <$> fileRead inputFile
  let res = "\nmodule Affirm (affirm) where\n\naffirm :: String\naffirm = \"" <> T.intercalate "\\n" lns <> "\"\n\n"
  fileWrite outputFile res

repText :: T.Text -> T.Text
repText = T.replace "」" ">" . T.replace "「" "<"

inputFile :: FilePath
inputFile = "Texts/affirm.txt"


outputFile :: FilePath
outputFile = "Affirm.hs"
