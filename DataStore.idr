module Main

import Data.Vect
import Data.Strings
import System.REPL

data DataStore : Type where
  MkData : (size : Nat) ->
           (items : Vect size String) ->
           DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items') newitem = MkData _ (addToData items')
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newitem]
    addToData (item :: items) = item :: addToData items

data Command = Add String
             | Get Integer
             | Quit
             | Size
             | Search String

parseCommand : (input : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              True => Nothing
                              False => Just (Get (cast val))
parseCommand "quit" "" = Just Quit
parseCommand "size" "" = Just Size
parseCommand "search" sub_str = Just (Search sub_str)
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (input : String) ->
           (store : DataStore) ->
           (pos : Integer) ->
           Maybe (String, DataStore)
getEntry input store pos
  = let store_items = items store in
        case integerToFin pos (size store) of
             Nothing => Just ("Out of range\n", store)
             Just id => Just (index id store_items ++ "\n", store)

searchString : Nat -> (items : Vect n String) -> (str : String) -> String
searchString idx [] str = ""
searchString idx (x :: xs) str
    = let rest = searchString (idx + 1) xs str in
      if isInfixOf str x
         then show idx ++ ": " ++ x ++ "\n" ++ rest
         else rest

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp
  = case parse inp of
         Nothing => Just ("Invalid Command\n", store)
         (Just (Add item)) =>
            Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
         (Just (Get x)) => getEntry inp store x
         (Just Quit) => Nothing
         (Just Size) => Just ("Current size: " ++ show (size store) ++ "\n", store)
         (Just (Search str)) => Just (searchString 0 (items store) str, store)

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
