MFLAGS = -s
MAKEFLAGS = $(MFLAGS)
SANDBOX = .cabal-sandbox
IDRIS = ${SANDBOX}/bin/idris
IDRIS_NETWORK = ${SANDBOX}/share/x86_64-osx-ghc-7.6.1/idris-0.9.12/network

.PHONY: repl standalone effect play cli net

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
