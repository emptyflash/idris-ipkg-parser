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

%access public

identifier : Parser String
identifier = map pack (some (satisfy isAlpha)) <$ space
             <?> "Identifier"

comma : Parser ()
comma = char ',' <$ space
        <?> "Comma"

equals : Parser ()
equals = char '=' <$ space
         <?> "comma"

-- Inspired by Json.idr in Lightyear examples

private
specialChar : Parser Char
specialChar = do
  c <- satisfy (const True)
  case c of
    '\\' => pure '\\'
    '/'  => pure '/'
    '.'  => pure '.'
    _    => satisfy (const False) <?> "expected special path char"

private
pathChar : Parser Char
pathChar = specialChar <|> satisfy isAlpha <?> "Char in FilePath" 

filepath : Parser String
filepath = map pack (some pathChar) <$ space
         <?> "filepath"

-- The following is 'inspired' from Bibdris

private
lit : Char -> Char -> Parser String
lit l r = char l $> (map pack . many $ satisfy (/= r)) <$ char r


stringLiteral : Parser String
stringLiteral = lit '"' '"' <?> "string literal"


-- --------------------------------------------------------------------- [ EOF ]
