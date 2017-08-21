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

joinWith : String -> List String -> String
joinWith sep = foldl (++) "" . intersperse sep

showIPackageEntry : IPackageEntry -> String
showIPackageEntry (IPkgName x)     = "package " ++ x
showIPackageEntry (IPkgModules xs) = "modules = " ++ joinWith ", " xs
showIPackageEntry (IPkgSrcDir x)   = "sourcedir = " ++ x
showIPackageEntry (IPkgExe x)      = "executable = " ++ x
showIPackageEntry (IPkgMain x)     = "main = " ++ x
showIPackageEntry (IPkgOpts x)     = "opts = " ++ x
showIPackageEntry (IPkgMake x)     = "makefile = " ++ x
showIPackageEntry (IPkgLibs xs)    = "libs = " ++ joinWith ", " xs
showIPackageEntry (IPkgObjs xs)    = "objs = " ++ joinWith ", " xs
showIPackageEntry (IPkgPkgs xs)    = "pkgs = " ++ joinWith ", " xs

Show IPackageEntry where
  show = showIPackageEntry

-- --------------------------------------------------------------- [ iPkg File ]
-- iPkg Files are just a list of entries.
-- @TODO Turn in to Record

data IPkgFile = MkIPkgFile (List IPackageEntry)

showIPkgFile : IPkgFile -> String
showIPkgFile (MkIPkgFile xs) = joinWith "\n" $ map show xs

Show IPkgFile where
  show = showIPkgFile


-- --------------------------------------------------------------------- [ EOF ]
