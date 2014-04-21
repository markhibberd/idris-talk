MFLAGS = -s
MAKEFLAGS = $(MFLAGS)
SANDBOX = .cabal-sandbox

.PHONY: prepare


prepare: ${SANDBOX}
	test -x ${SANDBOX}/bin/alex || cabal install alex
	test -x ${SANDBOX}/bin/idris || PATH=${SANDBOX}/bin:$$PATH cabal install idris-0.9.12


${SANDBOX}:
	cabal sandbox init
