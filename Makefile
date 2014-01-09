## ---------------------------------------------------- [ iPkg Parser Makefile ]
##
## Author: Jan de Muijnck-Hughes
##   Date: 2014-01-09
##

IDRIS := idris
PKG   := ipkgparser

build: .PHONY
	$(IDRIS) --build ${PKG}.ipkg

clean: .PHONY
	$(IDRIS) --clean ${PKG}.ipkg

rebuild: clean build

.PHONY:

## --------------------------------------------------------------------- [ EOF ]
