module Main (main) where

import Options.Applicative hiding (infoParser)

type ItemIndex = Int
type ItemDescription = Maybe String

defaultDataPath :: FilePath
defaultDataPath = "~/.to-do.yaml"

infoParser :: Parser Command
infoParser = pure Info

initParser :: Parser Command
initParser = pure List

listParser :: Parser Command
listParser = pure List

addParser :: Parser Command
addParser = pure Add

viewParser :: Parser Command
viewParser = pure View

updateParser :: Parser Command
updateParser = pure Update

removeParser :: Parser Command
removeParser = pure Remove

commandParser :: Parser Command
commandParser = subparser $ mconcat
  [ command "info"   (info infoParser   (progDesc "Show info"))
  , command "init"   (info initParser   (progDesc "Initialize items"))
  , command "list"   (info listParser   (progDesc "List items"))
  , command "add"    (info addParser    (progDesc "Add item"))
  , command "view"   (info viewParser   (progDesc "View item"))
  , command "update" (info updateParser (progDesc "Update item"))
  , command "remove" (info removeParser   (progDesc "Remove item"))
  ]


dataPathParser :: Parser FilePath
dataPathParser = strOption $
  value defaultDataPath
  <> long "data-path"
  <> short 'p'
  <> metavar "DATAPATH"
  <> help("path to data file (default " ++ defaultDataPath ++ ")")

itemIndexParser :: Parser ItemIndex
itemIndexParser = argument auto (metavar "ITEMINDEX" <> help "index of item")

data Options = Options FilePath ItemIndex ItemDescription deriving Show

data Command =
  Info
    | Init
    | List
    | Add
    | View
    | Update
    | Remove

itemDescriptionValueParser :: Parser String
itemDescriptionValueParser =
  strOption (
    long "desc"
    <> short 'd'
    <> metavar "DESCRIPTION"
    <> help "description"
  )

updateItemDescriptionParser :: Parser ItemDescription
updateItemDescriptionParser = 
  Just <$> itemDescriptionValueParser
  <|> flag' Nothing (long "clear-desc") -- "--clear-desc"

optionsParser :: Parser Options
optionsParser = Options
                <$> dataPathParser
                <*> itemIndexParser
                <*> updateItemDescriptionParser

main :: IO ()
main = do
  options <- execParser(info optionsParser (progDesc "To-do manager"))
  putStrLn $ "options=" ++ show options
