default: ehc_to_ghc_core.pdf

%.aux: %.tex

%.dvi: %.tex
	latex $<

%.ps: %.dvi
	dvips -f $< > $@

%.pdf: %.tex references.bib overview1.pdf
	pdflatex $< 
	bibtex $(<:.tex=)
	rm -f $(<:.tex=.ind)
	makeindex $(<)
	pdflatex $<

ehc_to_ghc_core.tex: ehc_to_ghc_core.ltex
	lhs2TeX --poly $< > $@
