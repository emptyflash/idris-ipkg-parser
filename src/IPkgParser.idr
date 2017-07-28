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
import Lightyear.Char

import IPkgParser.Model
import IPkgParser.EntryParser

%access public export

-- -------------------------------------------------------------- [ Parse File ]

parseIPkgFile : Parser IPkgFile
parseIPkgFile = do
  pname <- parseIPkgDec
  spaces
  content <- many parseIPkgEntry
  let f = [pname] ++ content
  pure (MkIPkgFile f) <?> "Parse iPkg File"

-- --------------------------------------------------------------------- [ EOF ]
