default: ehc_to_ghc_core.pdf

ehc_to_ghc_core.pdf: ehc_to_ghc_core.tex
	pdflatex $<

ehc_to_ghc_core.tex: ehc_to_ghc_core.ltex
	lhs2TeX --poly $< > $@
