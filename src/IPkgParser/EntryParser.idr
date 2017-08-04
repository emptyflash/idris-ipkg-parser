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
import Lightyear.Char

import IPkgParser.Model
import IPkgParser.Utils

%access public export

parseIPkgDec : Parser IPackageEntry
parseIPkgDec = do
  string "package"
  spaces
  name <- identifier
  pure (IPkgName name) <?> "Package Declaration"

parseIPkgEntry : Parser IPackageEntry
parseIPkgEntry = do
  e <- identifier
  space
  equals
  let result = case e of
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
      dir <- filepath
      pure $ IPkgSrcDir dir
    "modules" => do
      ms <- commaSep1 filepath
      pure $ IPkgModules ms
    "objs" => do
      os <- commaSep1 filepath
      pure $ IPkgObjs os
    "libs" => do
      ls <- commaSep1 filepath
      pure $ IPkgLibs ls
    "pkgs" => do
      ls <- commaSep1 identifier
      pure $ IPkgPkgs ls
  result <?> "iPkg Entries"

-- --------------------------------------------------------------------- [ EOF ]
