default: ehc_to_ghc_core.pdf

%.aux: %.tex
	latex $< 

%.dvi: %.tex
	latex $<

%.ps: %.dvi
	dvips -f $< > $@

%.bbl: %.aux
	bibtex $(<:.tex=)
	rm -f $(<:.tex=.ind)
	makeindex $(<:.aux=.tex)

%.pdf: %.tex %.bbl
	pdflatex $<

ehc_to_ghc_core.tex: ehc_to_ghc_core.ltex
	lhs2TeX --poly $< > $@
