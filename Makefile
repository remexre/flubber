all:
	cabal v2-build
watch:
	watchexec -cre cabal,hs $(MAKE)
.PHONY: all watch
