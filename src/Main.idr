-- ---------------------------------------------------------------- [ Main.idr ]
-- Module      : Main
-- Description : Sample usage of iPkg file parser.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Main

import System

import IPkgParser
import Lightyear.Strings

processArgs : List String -> Maybe String
processArgs []            = Nothing
processArgs (_ :: x ::xs) = Just x

processFile : String -> IO ()
processFile fname = do
  contents <- readFile fname
  case parse parseIPkgFile contents of
    Right res => putStrLn $ show res
    Left err  => putStrLn err

main : IO ()
main = do
  args <- getArgs
  case processArgs args of
    Nothing    => putStrLn "Boo Boo"
    Just fname => processFile fname

-- --------------------------------------------------------------------- [ EOF ]  
