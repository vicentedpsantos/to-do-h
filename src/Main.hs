module Main (main) where

import Options.Applicative

type ItemIndex = Int

defaultDataPath :: FilePath
defaultDataPath = "~/.to-do.yaml"

itemIndexParser :: Parser ItemIndex
itemIndexParser = argument auto (metavar "ITEMINDEX" <> help "index of item")

main :: IO ()
main = do
  itemIndex <- execParser (info itemIndexParser (progDesc "To-do manager"))
  putStrLn $ "itemIndex" ++ show itemIndex
