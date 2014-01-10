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
  name <- identifier
  pure (IPkgName name)
  <?> "Package Declaration"

parseIPkgEntry : Parser IPackageEntry
parseIPkgEntry = do
  e <- identifier
  space
  equals
  space
  case e of
    "main" => do
      fname <- identifier
      pure $ IPkgMain fname
    "opts" => do
      opts <- stringLiteral
      pure $ IPkgOpts opts
    "makefile" => do
      fname <- identifier
      pure $ IPkgMake fname
    "executable" => do
      name <- identifier
      pure $ IPkgExe name
    "sourcedir" => do
      dir <- identifier
      pure $ IPkgSrcDir dir
    "modules" => do
      ms <- sepBy1 filepath comma
      pure $ IPkgModules ms
    "objs" => do
      os <- sepBy1 filepath comma
      pure $ IPkgObjs os
    "libs" => do
      ls <- sepBy1 filepath comma
      pure $ IPkgLibs ls
  --parseIPkgEntry' <$ space
                 <?> "Lexing Contents"

-- --------------------------------------------------------------------- [ EOF ]
