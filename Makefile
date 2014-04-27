MFLAGS = -s
MAKEFLAGS = $(MFLAGS)
SANDBOX = .cabal-sandbox
IDRIS = ${SANDBOX}/bin/idris

.PHONY: repl

default: repl

repl: ${IDRIS}
	${IDRIS} -p effects  -i src src/TicTacType/Data.idr

${IDRIS}: ${SANDBOX}
	test -x ${SANDBOX}/bin/alex || cabal install alex
	test -x ${SANDBOX}/bin/idris || env PKG_CONFIG_PATH=/usr/local/Cellar/libffi/3.0.11/lib/pkgconfig PATH=${SANDBOX}/bin:$$PATH cabal install -f FFI idris-0.9.12

${SANDBOX}:
	cabal sandbox init
