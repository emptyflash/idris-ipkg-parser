-- ----------------------------------------------- [ Idris Package File Parser ]
-- A package file parser for Idris
module IPkgParser

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import Data.SortedMap

%access public

-- ------------------------------------------------------------------- [ Types ]
-- Entries found within a iPkg file.
data IPackageEntry = IPkgName String
                   | IPkgModules (List String)
                   | IPkgSrcDir String
                   | IPkgExe String
                   | IPkgMain String
                   | IPkgOpts String
                   | IPkgMake String
                   | IPkgLibs (List String)
                   | IPkgObjs (List String)

showIPackageEntry : IPackageEntry -> String
showIPackageEntry (IPkgName x)     = "package " ++ show x
showIPackageEntry (IPkgModules xs) = "modules = " ++ show xs
showIPackageEntry (IPkgSrcDir x)   = "sourcedir = " ++ show x
showIPackageEntry (IPkgExe x)      = "executable = " ++ show x
showIPackageEntry (IPkgMain x)     = "main = " ++ show x
showIPackageEntry (IPkgOpts x)     = "opts = " ++ show x
showIPackageEntry (IPkgMake x)     = "makefile = " ++ show x
showIPackageEntry (IPkgLibs xs)    = "libs = " ++ show xs
showIPackageEntry (IPkgObjs xs)    = "objs = " ++ show xs

instance Show IPackageEntry where
  show = showIPackageEntry

-- The file itself.
data IPkgFile = MkIPkgFile (List IPackageEntry)

showIPkgFile : IPkgFile -> String
showIPkgFile (MkIPkgFile xs) = show xs

instance Show IPkgFile where
  show = showIPkgFile

-- ------------------------------------------------------------------- [ Utils ]

parseString : Parser String
parseString = map pack (some (satisfy isAlpha))

parseStringLiteral : Parser String
parseStringLiteral = char '"' >! parseString <$ char '"'

-- ----------------------------------------------------------- [ Parse Entries ]

parseIPkgDec : Parser IPackageEntry
parseIPkgDec = do
  string "package"
  space
  name <- parseString
  pure $ IPkgName name
  <?> "Package Declaration"
  

-- -------------------------------------------------------------- [ Parse File ]
mutual

  parseIPkgEntry : Parser IPackageEntry
  parseIPkgEntry = parseIPkgDec <?> "Package Entry"

parseIPkgFile : Parser IPkgFile
parseIPkgFile = do
  pname <- parseIPkgDec
  pure $ MkIPkgFile [pname]
-- --------------------------------------------------------------------- [ EOF ]
