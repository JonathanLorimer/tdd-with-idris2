module Main

import Data.Vect
import Data.Strings
import Data.List
import System.REPL

infixr 5 .+.

data Schema = SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)

addToStore : (store : DataStore) -> (SchemaType (schema store)) -> DataStore
addToStore (MkData schema size store) newitem
  = MkData schema _ (addToData store)
  where
    addToData : Vect oldsize (SchemaType schema) -> Vect (S oldsize) (SchemaType schema)
    addToData [] = [newitem]
    addToData (item :: items) = item :: addToData items

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema
  = case size store of
         0 => Just (MkData schema _ [])
         (S k) => Nothing

data Command : Schema -> Type where
  SetSchema : (newSchema : Schema) -> Command schema
  Add       : SchemaType schema -> Command schema
  Get       : Integer -> Command schema
  Quit      : Command schema
  Size      : Command schema

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs)
      = case span (/= '"') xs of
             (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
             _ => Nothing
    getQuoted _ = Nothing
parsePrefix SInt input = case span isDigit input of
                             ("", rest) => Nothing
                             (num, rest) => Just (cast num, ltrim rest)
parsePrefix (schema1 .+. schema2) input
  = case parsePrefix schema1 input of
         Nothing => Nothing
         Just (val1, input') =>
              case parsePrefix schema2 input' of
                   Nothing => Nothing
                   Just (val2, input'') => Just ((val1, val2), input'')

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Just (res, "") => Just res
                                  Just _ => Nothing
                                  Nothing => Nothing

parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs)
  = case xs of
         [] => Just SString
         _ => case parseSchema xs of
                   Nothing => Nothing
                   Just xs_sch => Just (SString .+. xs_sch)
parseSchema ("Int" :: xs)
  = case xs of
         [] => Just SInt
         _ => case parseSchema xs of
                   Nothing => Nothing
                   Just xs_sch => Just (SInt .+. xs_sch)
parseSchema _ = Nothing


parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "add" rest
  = do entry <- parseBySchema schema rest
       Just (Add entry)
parseCommand schema "get" val
  = case all isDigit (unpack val) of
         False => Nothing
         True => Just (Get (cast val))
parseCommand schema "schema" rest
  = do schemaok <- parseSchema (words rest)
       Just (SetSchema schemaok)
parseCommand schema "quit" "" = Just Quit
parseCommand schema "size" "" = Just Size
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                   (cmd, args) => parseCommand schema cmd (ltrim args)

display : {schema : _} -> SchemaType schema -> String
display {schema = SString} x = x
display {schema = SInt} x = show x
display {schema = (y .+. z)} (iteml, itemr) = display iteml ++ ", " ++ display itemr

getEntry : (input : String) ->
           (store : DataStore) ->
           (pos : Integer) ->
           Maybe (String, DataStore)
getEntry input store pos
  = let store_items = items store in
        case integerToFin pos (size store) of
             Nothing => Just ("Out of range\n", store)
             Just id => Just (display (index id store_items) ++ "\n", store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp
  = case parse (schema store) inp of
         Nothing => Just ("Invalid Command\n", store)
         (Just (Add item)) =>
            Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
         (Just (Get x)) => getEntry inp store x
         (Just Quit) => Nothing
         (Just Size) => Just ("Current size: " ++ show (size store) ++ "\n", store)
         (Just (SetSchema schema')) =>
            case setSchema store schema' of
                 Nothing => Just ("Can't update schema\n", store)
                 Just store' => Just ("OK\n", store')

main : IO ()
main = replWith (MkData (SString .+. SString .+. SInt) _ []) "Command: " processInput
