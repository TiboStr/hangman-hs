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

main :: IO ()
main = do
  loop "aac" []

loop :: Text -> [Char] -> IO ()
loop word guesses = do
  Gtk.init Nothing

  win <- Gtk.windowNew Gtk.WindowTypeToplevel

  Gtk.setContainerBorderWidth win 10
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

  fillGrid grid ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'] win word guesses

  #attach grid wordDisplay 0 0 13 1  -- col row width height
  #attach grid alreadyGuessed 0 1 13 1
  #attach grid img 0 21 13 20
  #add win grid

  Gtk.onWidgetDestroy win Gtk.mainQuit
  #showAll win

  Gtk.main


fillGrid :: Gtk.Grid -> [Char] -> Gtk.Window -> Text -> [Char] -> IO ()
fillGrid grid letters win word guesses = do
  let rows = Data.List.splitAt (Data.List.length letters `div` 2) letters
  fillGridRow grid (fst rows) win word guesses 3 0
  fillGridRow grid (snd rows) win word guesses 4 0

  
fillGridRow :: Gtk.Grid -> [Char] -> Gtk.Window -> Text -> [Char] -> Int -> Int -> IO ()
fillGridRow grid letters win word guesses row index = do
  btn <- Gtk.buttonNew
  Gtk.buttonSetLabel btn $ toUpper $ pack [letters !! index]
  on btn #clicked $ do
    Gtk.windowClose win
    loop word $ letters !! index : guesses
  #attach grid btn (fromIntegral index) (fromIntegral row) 1 1

  Control.Monad.when (index < Data.List.length letters - 1) $ fillGridRow grid letters win word guesses row $ index + 1



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

