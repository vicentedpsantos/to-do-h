{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Exception
import           Data.Aeson hiding (Options)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.List.Safe ((!!))
import           Data.Functor
import           Data.String.Utils
import           Data.Time
import qualified Data.Yaml as Yaml
import           GHC.Generics
import           Options.Applicative hiding (infoParser)
import           Prelude hiding ((!!))
import           System.Directory
import           System.IO.Error

type ItemIndex       = Int
type ItemTitle       = String
type ItemDescription = Maybe String
type ItemPriority    = Maybe Priority
type ItemDueBy       = Maybe LocalTime

data Priority = Low | Normal | High deriving (Generic, Show)
instance ToJSON Priority
instance FromJSON Priority

newtype ToDoList = ToDoList [Item] deriving (Generic, Show)
instance ToJSON ToDoList
instance FromJSON ToDoList

data Options = Options FilePath Command deriving Show

data Command =
  Info
    | Init
    | List
    | Add Item
    | View ItemIndex
    | Update ItemIndex ItemUpdate
    | Remove ItemIndex
    deriving Show

data Item = Item
  { title       :: ItemTitle
  , description :: ItemDescription
  , priority    :: ItemPriority
  , dueBy       :: ItemDueBy
  } deriving (Generic, Show)
instance ToJSON Item
instance FromJSON Item

data ItemUpdate = ItemUpdate
  { titleUpdate       :: Maybe ItemTitle
  , descriptionUpdate :: Maybe ItemDescription
  , priorityUpdate    :: Maybe ItemPriority
  , dueByUpdate       :: Maybe ItemDueBy
  } deriving Show

defaultDataPath :: FilePath
defaultDataPath = "~/.to-do.yaml"

infoParser :: Parser Command
infoParser = pure Info

initParser :: Parser Command
initParser = pure List

listParser :: Parser Command
listParser = pure List

addParser :: Parser Command
addParser = Add <$> addItemParser

addItemParser :: Parser Item
addItemParser = Item
  <$> argument str (metavar "TITLE" <> help "title")
  <*> optional itemDescriptionValueParser
  <*> optional itemPriorityValueParser
  <*> optional itemDueByValueParser

viewParser :: Parser Command
viewParser = View <$> itemIndexParser

viewItemParser = Item
  <$> argument str (metavar "TITLE" <> help "title")

updateParser :: Parser Command
updateParser = Update <$> itemIndexParser <*> updateItemParser

updateItemParser :: Parser ItemUpdate
updateItemParser = ItemUpdate
  <$> optional updateItemTitleParser
  <*> optional updateItemDescriptionParser
  <*> optional updateItemPriorityParser
  <*> optional updateItemDueByParser

updateItemTitleParser :: Parser ItemTitle
updateItemTitleParser = itemTitleValueParser

updateItemDescriptionParser :: Parser ItemDescription
updateItemDescriptionParser = 
  Just <$> itemDescriptionValueParser
  <|> flag' Nothing (long "clear-desc") -- "--clear-desc"

updateItemPriorityParser :: Parser ItemPriority
updateItemPriorityParser = 
  Just <$> itemPriorityValueParser
  <|> flag' Nothing (long "clear-priority") -- "--clear-priority"

updateItemDueByParser :: Parser ItemDueBy
updateItemDueByParser = 
  Just <$> itemDueByValueParser
  <|> flag' Nothing (long "clear-due-by") -- "--clear-desc"

removeParser :: Parser Command
removeParser = Remove <$> itemIndexParser

commandParser :: Parser Command
commandParser = subparser $ mconcat
  [ command "info"   (info infoParser   (progDesc "Show info"))
  , command "init"   (info initParser   (progDesc "Initialize items"))
  , command "list"   (info listParser   (progDesc "List items"))
  , command "add"    (info addParser    (progDesc "Add item"))
  , command "view"   (info viewParser   (progDesc "View item"))
  , command "update" (info updateParser (progDesc "Update item"))
  , command "remove" (info removeParser (progDesc "Remove item"))
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

itemTitleValueParser :: Parser String
itemTitleValueParser =
  strOption(
    long "title"
    <> short 't'
    <> metavar "TITLE"
    <> help "title"
  )

itemDescriptionValueParser :: Parser String
itemDescriptionValueParser =
  strOption (
    long "desc"
    <> short 'd'
    <> metavar "DESCRIPTION"
    <> help "description"
  )

itemPriorityValueParser :: Parser Priority
itemPriorityValueParser =
  option readPriority(
    long "priority"
    <> short 'p'
    <> metavar "PRIORITY"
    <> help "priority"
  )
    where
      readPriority = eitherReader $ \arg ->
        case arg of
          "1" -> Right Low
          "2" -> Right Normal
          "3" -> Right High
          _   -> Left $ "Invalid priority value " ++ arg

itemDueByValueParser :: Parser LocalTime
itemDueByValueParser =
  option readDateTime(
    long "due-by"
    <>  short 'b'
    <> metavar "DUEBY"
    <> help "due-by date/time"
  )
    where
      readDateTime = eitherReader $ \arg ->
        case parseDateTimeMaybe arg of
          (Just dateTime) -> Right dateTime
          Nothing -> Left $ "Date/time string must be in " ++ dateTimeFormat ++ " format"
      parseDateTimeMaybe = parseTimeM False defaultTimeLocale dateTimeFormat
      dateTimeFormat = "%Y/%m/%d %H:%M:%S"


optionsParser :: Parser Options
optionsParser = Options
                <$> dataPathParser
                <*> commandParser

main :: IO ()
main = do
  Options dataPath command <- execParser(info optionsParser (progDesc "To Do List"))
  homeDir <- getHomeDirectory
  let expandedDataPath = replace "~" homeDir dataPath
  run expandedDataPath command

run :: FilePath -> Command -> IO ()
run dataPath Info         = putStrLn "info"
run dataPath Init         = putStrLn "init"
run dataPath List         = putStrLn "list"
run dataPath (Add item)   = addItem dataPath item
run dataPath (View idx)   = viewItem dataPath idx
run dataPath (Update idx itemUpdate) = putStrLn $ "update: idx=" ++ show idx ++ " itemUpdate=" ++ show itemUpdate
run dataPath (Remove idx) = putStrLn $ "remove: idx=" ++ show idx

writeToDoList :: FilePath -> ToDoList -> IO ()
writeToDoList dataPath toDoList = BS.writeFile dataPath (Yaml.encode toDoList)

readToDoList :: FilePath -> IO ToDoList
readToDoList dataPath = do
  mbToDoList <- catchJust
    (\e -> if isDoesNotExistError e then Just () else Nothing)
    (BS.readFile dataPath <&> Yaml.decode)
    (\_ -> return $ Just (ToDoList []))
  case mbToDoList of
    Nothing -> error "YAML file is corrupt"
    Just toDoList -> return toDoList

showField ::(a -> String) -> Maybe a -> String
showField f (Just x) = f x
showField _ Nothing = "(not set)"

showItem :: ItemIndex -> Item -> IO ()
showItem idx (Item title mbDescription mbPriority mbDueBy) = do
  putStrLn $ "[" ++ show idx ++ "]: " ++ title
  putStr " Description: "
  putStrLn $ showField id mbDescription
  putStr " Priority: "
  putStrLn $ showField show mbPriority
  putStr " Due by: "
  putStrLn $ showField (formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S") mbDueBy

viewItem :: FilePath -> ItemIndex -> IO ()
viewItem dataPath idx = do
  ToDoList items <- readToDoList dataPath
  let mbItem = items !! idx
  case mbItem of
    Nothing -> putStrLn "Invalid item index"
    Just item -> showItem idx item
  print items

addItem :: FilePath -> Item -> IO ()
addItem dataPath item = do
  ToDoList items <- readToDoList dataPath
  let newToDoList = ToDoList (item : items)
  writeToDoList dataPath newToDoList
