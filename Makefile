SOURCES = IR.hs Fhwc.hs IR.hs IRUtils.hs IRInterpreter.hs IRTransforms.hs \
	  CombinationPass.hs RemoveRecursionPass.hs CoreToIR.hs \
	  IRToHaskell.hs IRToOCaml.hs

fhwc : $(SOURCES)
	ghc --make -W -Wall -package ghc -o fhwc Fhwc.hs

%.o : %.hs
	ghc --make -Wall -W -c $<

.PHONY : clean
clean :
	rm *.hi *.o fhwc
