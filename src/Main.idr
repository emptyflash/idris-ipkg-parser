-- ---------------------------------------------------------------- [ Main.idr ]
-- Module      : Main
-- Description : Sample usage of iPkg file parser.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Main

import System

import IPkgParser
import IPkgParser.Model
import Lightyear.Strings

processArgs : List String -> Maybe String
processArgs []            = Nothing
processArgs (_ :: x ::xs) = Just x
processArgs _             = Nothing

processFile : String -> IO ()
processFile fname = do
  Right contents <- readFile fname | Left err => (putStrLn $ "Problem reading file: " ++ show err)
  case Strings.parse parseIPkgFile contents of
    Right res => putStrLn $ show res
    Left err  => putStrLn err

main : IO ()
main = do
  args <- getArgs
  case processArgs args of
    Nothing    => putStrLn "Boo Boo"
    Just fname => processFile fname

-- --------------------------------------------------------------------- [ EOF ]  
