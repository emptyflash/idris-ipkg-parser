-- -------------------------------------------------------------- [ Parser.idr ]
-- Module      : IPkgParser.EntryParser
-- Description : Parsing functions for iPkg entries.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module IPkgParser.EntryParser

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import IPkgParser.Model
import IPkgParser.Utils

%access public


parseIPkgDec : Parser IPackageEntry
parseIPkgDec = do
  string "package"
  space
  name <- stringLiteral
  pure $ IPkgName name
  <?> "Package Declaration"

-- Parse List Style Entries
  
parseIPkgModules : Parser IPackageEntry
parseIPkgModules = do
  string "modules"
  space
  equals
  space
  ms <- sepBy1 stringLiteral comma
  pure $ IPkgModules ms
  <?> "Modules"
  
parseIPkgLibs : Parser IPackageEntry
parseIPkgLibs = do
  string "libs"
  space
  equals
  space
  ls <- sepBy1 stringLiteral comma
  pure $ IPkgLibs ls
  <?> "Libraries"

parseIPkgObjs : Parser IPackageEntry
parseIPkgObjs = do
  string "objs"
  space
  equals
  space
  os <- sepBy1 stringLiteral comma
  pure $ IPkgObjs os
  <?> "Objects"

-- Parse Key String entries

parseIPkgSDir : Parser IPackageEntry
parseIPkgSDir = do
  string "sourcedir"
  space
  equals
  space
  dir <- stringLiteral
  pure $ IPkgSrcDir dir
  <?> "Source Dir"

parseIPkgExe : Parser IPackageEntry
parseIPkgExe = do
  string "executable"
  space
  equals
  space
  name <- stringLiteral
  pure $ IPkgExe name
  <?> "Exe Name"

parseIPkgMain : Parser IPackageEntry
parseIPkgMain = do
  string "main"
  space
  equals
  space
  func <- stringLiteral
  pure $ IPkgMain func
  <?> "Main Function"

parseIPkgMake : Parser IPackageEntry
parseIPkgMake = do
  string "makefile"
  space
  equals
  space
  fname <- stringLiteral
  pure $ IPkgMake fname
  <?> "Makefile"

-- Parse Key String Literal Entries

parseIPkgOpts : Parser IPackageEntry
parseIPkgOpts = do
  string "opts"
  space
  equals
  space
  opts <- parseQuotedString
  pure $ IPkgOpts opts
  <?> "Build Opts"

-- --------------------------------------------------------------------- [ EOF ]
