all:
	flock .build-lock cabal v2-build
run:
	flock .build-lock cabal v2-run -- flubber flubber.toml
watch:
	watchexec -cre cabal,hs $(MAKE)
.PHONY: all watch
