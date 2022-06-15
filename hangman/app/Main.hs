{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.GI.Base
import           Data.List
import           Data.Text
import           Data.Text.Internal
import           System.Directory
import qualified GI.Gtk             as Gtk
import GHC.Base (undefined)

data State = Playing Text [Char] | Won Text | Lost Text

main :: IO ()
main = do
  loop (Playing "aac" [])

loop :: State -> IO ()
loop (Won word) = do
  Gtk.init Nothing

  win <- Gtk.windowNew Gtk.WindowTypeToplevel

  lbl <- Gtk.labelNew Nothing
  Gtk.labelSetMarkup lbl $ append "You won!, You guessed the word \"" $ append word "\" correctly."

  #add win lbl

  Gtk.onWidgetDestroy win Gtk.mainQuit
  #showAll win
  Gtk.main

loop (Lost word) = do
  Gtk.init Nothing

  win <- Gtk.windowNew Gtk.WindowTypeToplevel

  lbl <- Gtk.labelNew Nothing
  Gtk.labelSetMarkup lbl $ append "You lose., You couldn't guess the word \"" $ append word  "\"."

  #add win lbl

  Gtk.onWidgetDestroy win Gtk.mainQuit
  #showAll win
  Gtk.main


loop (Playing word guesses) = do
  Gtk.init Nothing

  win <- Gtk.windowNew Gtk.WindowTypeToplevel

  Gtk.setContainerBorderWidth win 10  -- TODO: in een functie steken
  Gtk.setWindowTitle win "Hangman"
  Gtk.setWindowResizable win False
  Gtk.setWindowDefaultWidth win 750
  Gtk.setWindowDefaultHeight win 750
  Gtk.setWindowWindowPosition win Gtk.WindowPositionCenter

  wordDisplay <- Gtk.labelNew Nothing
  Gtk.labelSetMarkup wordDisplay $
    append "<span size=\"200%\" weight=\"heavy\" text_transform=\"uppercase\" letter_spacing=\"2500\">" $ append (hideGuessedCharsInWord word guesses) "</span>"

  alreadyGuessed <- Gtk.labelNew Nothing
  Gtk.labelSetMarkup alreadyGuessed $ pack $ "Already guessed: " ++ Data.List.intercalate ", " ( sort $ Data.List.map (\c -> [c]) guesses)

  currentDirectory <- getCurrentDirectory
  img <- Gtk.imageNewFromFile $ currentDirectory ++ "/images/hangman" ++ show (numberOfMisses word guesses) ++ ".png"

  grid <- Gtk.gridNew
  Gtk.gridSetColumnSpacing grid 10
  Gtk.gridSetRowSpacing grid 10
  Gtk.gridSetColumnHomogeneous grid True

  fillGrid grid ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'] win (Playing word guesses)

  #attach grid wordDisplay 0 0 13 1  -- col row width height
  #attach grid alreadyGuessed 0 1 13 1
  #attach grid img 0 21 13 20
  #add win grid

  Gtk.onWidgetDestroy win Gtk.mainQuit
  #showAll win

  Gtk.main


fillGrid :: Gtk.Grid -> [Char] -> Gtk.Window -> State -> IO ()
fillGrid grid letters win (Playing word guesses) = do
  let rows = Data.List.splitAt (Data.List.length letters `div` 2) letters
  fillGridRow grid (fst rows) win (Playing word guesses) 3 0
  fillGridRow grid (snd rows) win (Playing word guesses) 4 0

fillGrid _ _ _ _ = undefined


fillGridRow :: Gtk.Grid -> [Char] -> Gtk.Window -> State -> Int -> Int -> IO ()
fillGridRow grid letters win (Playing word guesses) row index = do
  btn <- Gtk.buttonNew
  Gtk.buttonSetLabel btn $ toUpper $ pack [letters !! index]
  on btn #clicked $ onButtonClick win (letters !! index) word guesses
  #attach grid btn (fromIntegral index) (fromIntegral row) 1 1

  Control.Monad.when (index < Data.List.length letters - 1) $ fillGridRow grid letters win (Playing word guesses) row $ index + 1

fillGridRow _ _ _ _ _ _ = undefined

onButtonClick :: Gtk.Window -> Char -> Text -> [Char] -> IO ()
onButtonClick win letter word guesses = do
  Gtk.windowClose win
  let newGuesses = if letter `Data.List.elem` guesses then  guesses else letter : guesses

  let action | numberOfMisses word guesses >= 8 = loop (Lost word)
             | hasWon word newGuesses = loop (Won word)
             | otherwise = loop (Playing word newGuesses)
  action

hideGuessedCharsInWord :: Text -> [Char] -> Text
hideGuessedCharsInWord word guessed =
  Data.Text.map
    (\c ->
       if c `Data.Text.elem` pack guessed
         then c
         else '_')
    word


numberOfMisses :: Text -> [Char] -> Int
numberOfMisses word guessed =
    Data.Text.length $ Data.Text.filter (\c -> not $ c `Data.Text.elem` word) $ pack guessed


hasWon :: Text -> [Char] -> Bool
hasWon word guessed = Data.Text.null $ Data.Text.filter (`notElem` guessed) word
