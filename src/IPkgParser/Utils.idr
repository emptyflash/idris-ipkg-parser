-- --------------------------------------------------------------- [ Utils.idr ]
-- Module      : IPkgParser.Utils
-- Description : Utility functions for helping with Parsing.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Utils

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings
import Lightyear.Char

%access public export

identifier : Parser String
identifier = map pack (some (satisfy $ isAlpha)) <?> "Identifier"

-- Inspired by Json.idr in Lightyear examples

specialChar : Parser Char
specialChar = do
  c <- anyChar
  case c of
    '\\' => pure '\\'
    '/'  => pure '/'
    '.'  => pure '.'
    _    => satisfy (const False)

pathChar : Parser Char
pathChar = specialChar <|> satisfy isAlpha 

filepath : Parser String
filepath = map pack (some pathChar) <?> "filepath"

-- The following is 'inspired' from Bibdris

stringLiteral : Parser String
stringLiteral = quoted '"' <?> "string literal"

-- --------------------------------------------------------------------- [ EOF ]
