MFLAGS = -s
MAKEFLAGS = $(MFLAGS)
SANDBOX = .cabal-sandbox
IDRIS = ${SANDBOX}/bin/idris
IDRIS_NETWORK = ${SANDBOX}/share/x86_64-osx-ghc-7.6.1/idris-0.9.12/network
DIST = dist

.PHONY: repl standalone effect play cli net serve

default: repl

repl: ${IDRIS}
	${IDRIS} -p effects  -i src src/TicTacType/Data.idr

standalone: ${IDRIS}
	${IDRIS} src/TicTacType/StandAlone.idr

effect: ${IDRIS}
	${IDRIS} -p effects -i src src/TicTacType/Effect.idr

play: ${IDRIS}
	${IDRIS} -p effects -i src src/TicTacType/Play.idr

cli: ${IDRIS}
	${IDRIS} -p effects -i src src/TicTacType/Cli.idr

net: ${IDRIS} ${IDRIS_NETWORK}
	${IDRIS} -p effects -p network  -i src src/TicTacType/Network.idr

dist/serve: ${IDRIS} ${IDRIS_NETWORK} ${DIST} src/serve.idr src/TicTacType/Network.idr
	rm dist/serve
	${IDRIS} -p effects -p network  -i src -o dist/serve src/serve.idr

serve: dist/serve
	dist/serve

network/network.ipkg:
	git submodule init
	git submodule update

${IDRIS_NETWORK}: ${IDRIS}
	(cd network && export PATH=../${SANDBOX}/bin:$$PATH && make all)

${IDRIS}: ${SANDBOX}
	test -x ${SANDBOX}/bin/alex || cabal install alex
	test -x ${SANDBOX}/bin/idris || env PKG_CONFIG_PATH=/usr/local/Cellar/libffi/3.0.11/lib/pkgconfig PATH=${SANDBOX}/bin:$$PATH cabal install -f FFI idris-0.9.12

${SANDBOX}:
	cabal sandbox init

${DIST}:
	mkdir -p $@
