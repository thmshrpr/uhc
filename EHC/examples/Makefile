GHC=ghc
EHC=../8_1/ehc
GHC_FLAGS=-O0
EHC_FLAGS=-pno

EH_SOURCES=$(shell echo *.eh)
HS_SOURCES=$(shell echo *.hs)

#Poor mans help message
default: 
	@cat Makefile

%.hcr: %.hs Makefile
	${GHC} -C -o /dev/null -fext-core ${GHC_FLAGS} $< && rm $(<:.hs=.hi)
	
%.code : %.eh ${EHC} Makefile
	${EHC} ${EHC_FLAGS} -ccode $<

%.hcr : %.eh ${EHC} Makefile
	${EHC} ${EHC_FLAGS} -cghc-core $<

%.run : %.hcr Makefile
	rm -f Main.o Main.hi
	${GHC} $< Main.hs -o $(@:.run=)
	./$(@:.run=)

% : %.hcr Makefile
	rm -f Main.o Main.hi
	${GHC} $< Main.hs -o $@

clean: Makefile
	rm -f $(HS_SOURCES:.hs=.hcr) $(EH_SOURCES:.eh=.hcr)
	rm -f $(HS_SOURCES:.hs=.hi)  $(EH_SOURCES:.eh=.hi)
	rm -f $(HS_SOURCES:.hs=.o)   $(EH_SOURCES:.eh=.o)
	rm -f $(HS_SOURCES:.hs=)   $(EH_SOURCES:.eh=)
	rm -f $(EH_SOURCES:.eh=.code)
