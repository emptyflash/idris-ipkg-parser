-- ---------------------------------------------------------- [ IPkgParser.idr ]
-- Module      : IPkgParser
-- Description : A package file parser for Idris
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module IPkgParser

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import IPkgParser.Model
import IPkgParser.EntryParser

%access public

-- -------------------------------------------------------------- [ Parse File ]
private
parseIPkgEntry' : Parser IPackageEntry
parseIPkgEntry' = parseIPkgModules
              <|> parseIPkgLibs
              <|> parseIPkgMake
              <|> parseIPkgObjs
              <|> parseIPkgSDir
              <|> parseIPkgExe
              <|> parseIPkgMain
              <|> parseIPkgOpts
              <?> "Package File Contents"

private
parseIPkgEntry : Parser IPackageEntry
parseIPkgEntry = parseIPkgEntry' <$ space
                 <?> "Lexing Contents"

parseIPkgFile : Parser IPkgFile
parseIPkgFile = do
  pname <- parseIPkgDec
  space
  content <- many parseIPkgEntry
  let f = [pname] ++ content
  pure $ MkIPkgFile f
  <?> "Parse iPkg File"

-- --------------------------------------------------------------------- [ EOF ]
