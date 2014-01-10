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

stringLiteral : Parser String
stringLiteral = map pack (some (satisfy isAlpha)) <$ space
              <?> "String"

comma : Parser ()
comma = char ',' <$ space
        <?> "Comma"

equals : Parser ()
equals = char '=' <$ space
         <?> "comma"

-- The following is 'inspired' from JSon.idr in lightyear examples

private
stringContents : Parser (List Char)
stringContents = (char '"' $!> pure []) <|> do
  c <- satisfy (/='"')
  map (c::) stringContents
  <?> "String Literal Contents"

-- Parse anything in between the quotes
parseQuotedString : Parser String
parseQuotedString = char '"' >! map pack stringContents
                     <?> "String Literal"

-- --------------------------------------------------------------------- [ EOF ]
