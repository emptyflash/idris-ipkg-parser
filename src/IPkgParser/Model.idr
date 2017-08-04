-- --------------------------------------------------------------- [ Model.idr ]
-- Module      : IPkgParser.Model
-- Description : Model for iPkg Files
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module IPkgParser.Model

%access public export

-- ------------------------------------------------------------ [ iPkg Entries ]

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
                   | IPkgPkgs (List String)

intercalate' : String -> List String -> String
intercalate' sep [] = ""
intercalate' sep [x] = x
intercalate' sep (x :: xs) = x ++ sep ++ intercalate' sep xs

showIPackageEntry : IPackageEntry -> String
showIPackageEntry (IPkgName x)     = "package " ++ show x
showIPackageEntry (IPkgModules xs) = "modules = " ++ intercalate' ", " xs
showIPackageEntry (IPkgSrcDir x)   = "sourcedir = " ++ show x
showIPackageEntry (IPkgExe x)      = "executable = " ++ show x
showIPackageEntry (IPkgMain x)     = "main = " ++ show x
showIPackageEntry (IPkgOpts x)     = "opts = " ++ show x
showIPackageEntry (IPkgMake x)     = "makefile = " ++ show x
showIPackageEntry (IPkgLibs xs)    = "libs = " ++ intercalate' ", " xs
showIPackageEntry (IPkgObjs xs)    = "objs = " ++ intercalate' ", " xs
showIPackageEntry (IPkgPkgs xs)    = "pkgs = " ++ intercalate' ", " xs

Show IPackageEntry where
  show = showIPackageEntry

-- --------------------------------------------------------------- [ iPkg File ]
-- iPkg Files are just a list of entries.
-- @TODO Turn in to Record

data IPkgFile = MkIPkgFile (List IPackageEntry)

showIPkgFile : IPkgFile -> String
showIPkgFile (MkIPkgFile xs) = show xs

Show IPkgFile where
  show = showIPkgFile


-- --------------------------------------------------------------------- [ EOF ]
