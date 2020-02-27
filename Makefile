compile:
	stack build uKanren-transformation --ghc-options="-j -rtsopts"

profile:
	stack build uKanren-transformation --ghc-options="-j" --profile

clean:
	stack clean

run:
	stack exec -- cpd +RTS -H128m -K64m -RTS

profile_run:
	stack exec --profile -- cpd +RTS -p
