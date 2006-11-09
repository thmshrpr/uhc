###############################################################################
# Main document name
DOC=presentation
# Individual subdocuments 
SUBDOCS:=
# Images (for dependencies)
IMAGES:=
# Extensions of auxiliary files generated (for the main document)
AUXEXTS=aux nav out snm toc vrb

# All auxiliary files; the same auxiliary files with an '.old' suffix;
# All intermediate files; all final target files.
AUXFILES=$(addprefix $(DOC)., $(AUXEXTS))  $(SUBDOCS:.tex=.aux)
OLDAUXFILES=$(addsuffix .old,  $(AUXFILES))
INTERMEDIATES=$(AUXFILES) $(OLDAUXFILES) $(DOC).log $(DOC).dvi
TARGETS=$(addprefix $(DOC)., ps pdf 2up.ps psnup.ps booklet.ps)

###############################################################################
# Programs and options
PDFLATEX=pdflatex
LATEX=latex
PDFLATEXOPTS=--interaction=nonstopmode
LATEXOPTS=--interaction=nonstopmode
DVIPS=dvips

###############################################################################
# repeatUntilDone -- runs TeX until all auxiliary files remain unchanged 
#    $(1):  name of TeX processor (e.g. 'latex' or 'pdflatex')
#
repeatUntilDone=\
	touch $(AUXFILES); REPEAT=y;\
	while [ "$$REPEAT" = "y" ]; do\
		for x in $(AUXFILES); do cp -f $$x $$x.old; done;\
		$(1) $(DOC).tex; REPEAT=n;\
		for x in $(AUXFILES); do cmp -s $$x $$x.old || REPEAT=y; done;\
	done;\
	$(RM) $(OLDAUXFILES)

###############################################################################

.PHONY: default pdf ps clean cleanall view warn todo 2up psnup booklet

default: pdf

pdf: $(DOC).pdf

ps: $(DOC).ps

$(DOC).pdf: $(DOC).tex $(SUBDOCS) $(IMAGES)
	$(call repeatUntilDone, $(PDFLATEX) $(PDFLATEXOPTS))

$(DOC).dvi: $(DOC).tex $(SUBDOCS) $(IMAGES)
	$(call repeatUntilDone, $(LATEX) $(LATEXOPTS))

$(DOC).ps: $(DOC).dvi
	$(DVIPS) -o $(DOC).ps $(DOC).dvi

clean:
	$(RM) $(INTERMEDIATES)

cleanall: clean
	$(RM) $(TARGETS)

###############################################################################

view: $(DOC).pdf
	ggv $(DOC).pdf

warn: $(DOC).pdf
	-@clear
	@echo
	@grep Warning $(DOC).log

todo:
	-@clear
	@echo
	@grep -i TODO $(DOC).tex $(SUBDOCS)

2up: $(DOC).ps
	cat $(DOC).ps | mpage -dp -bA4 -2 -t > $(DOC).2up.ps

psnup: $(DOC).ps
	cat $(DOC).ps | psnup -pa4 -2 > $(DOC).psnup.ps

booklet: $(DOC).ps
	cat $(DOC).ps | psbook | psnup -pa4 -2 | psset -t > $(DOC).booklet.ps

