slides: the_presentation.pdf

%.aux: %.tex

%.dvi: %.tex
	latex $<

%.ps: %.dvi
	dvips -f $< > $@

%.pdf: %.ps
	ps2pdf $<

the_presentation.tex: the_presentation.ltex overview1.latex
	lhs2TeX --poly $< > $@
