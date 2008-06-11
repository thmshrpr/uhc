LLVM_CODE_IMG_PREFIX       := $(TOP_PREFIX)text/llvm/img/
LLVM_CODE_SRC_PREFIX       := $(TOP_PREFIX)text/llvm/code/

LLVM_THESIS_EXAMPLES       := $(TEXT_TMP_VARIANT_PREFIX)Fib.lhs \
                              $(TEXT_TMP_VARIANT_PREFIX)FibExe.core \
                              $(TEXT_TMP_VARIANT_PREFIX)FibExe.grin \
                              $(TEXT_TMP_VARIANT_PREFIX)FibExe.sil \
                              $(TEXT_TMP_VARIANT_PREFIX)FibExe-opt.grin \
                              $(TEXT_TMP_VARIANT_PREFIX)GRIN_Fib_Tree.tex \
                              $(TEXT_TMP_VARIANT_PREFIX)GRIN_Fib_Tree_Opt.tex \
                              $(TEXT_TMP_VARIANT_PREFIX)LLVMExample.c \
                              $(TEXT_TMP_VARIANT_PREFIX)GetElementPtrExample.ll \
                              $(TEXT_TMP_VARIANT_PREFIX)PhiExample.ll \
                              $(TEXT_TMP_VARIANT_PREFIX)LLVMExample.ll  

LLVM_GRIN_FILES            := $(LLVM_CODE_SRC_PREFIX)FibExe.grin
LLVM_GRIN_FILES_DEP        := $(LLVM_CODE_SRC_PREFIX)FibExe-013-renameuniform.grin \
                              $(LLVM_CODE_SRC_PREFIX)Eval.grin

LLVM_CODE_FILES            := $(LLVM_CODE_SRC_PREFIX)FibExe.hs 
LLVM_CODE_FILES_DEP        := $(LLVM_CODE_SRC_PREFIX)Fib.hs \
                              $(LLVM_CODE_SRC_PREFIX)PreludeEHC8.hs

LLVM_SILLY_FILES           := $(LLVM_CODE_SRC_PREFIX)FibExe.sil 
LLVM_SILLY_FILES_DEP       := $(LLVM_CODE_SRC_PREFIX)FibExe-205.sil

text-variant-llvm: $(LLVM_THESIS_EXAMPLES)
	$(MAKE) TEXT_CFG_FIGS_INCLUDES_DOT_SRC=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=blockstyle --set=inclTOC --set=useHyperref --set=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=35 \
	  text-variant-dflt-bib

$(TEXT_TMP_VARIANT_PREFIX)%.lhs: $(TEXT_TMP_VARIANT_PREFIX)%.hs  
	echo '\\begin{code}' > $@
	cat $< >> $@
	echo '\end{code}' >> $@

$(TEXT_TMP_VARIANT_PREFIX)%: $(LLVM_CODE_SRC_PREFIX)%
	mkdir -p $(dir $@)
	cp $< $@

$(TEXT_TMP_VARIANT_PREFIX)%: $(LLVM_CODE_IMG_PREFIX)%
	mkdir -p $(dir $@)
	cp $< $@

$(LLVM_CODE_SRC_PREFIX)FibExe.core $(LLVM_CODE_SRC_PREFIX)FibExe-012-aliaselim.grin: $(LLVM_CODE_FILES)
	cd $(LLVM_CODE_SRC_PREFIX) ; \
	../../../bin/8_2/ehc -clexe --gen-cmt=0 --dump-grin-stages=1 --optimise=1 --priv=1 -p- $< && \
	sed -i 's/e10/undefined/g' FibExe.core

$(LLVM_CODE_SRC_PREFIX)FibExe-opt.grin: $(LLVM_CODE_SRC_PREFIX)FibExe-179-final.grin
	sed -i 's/fun_x_[0-9]\+_//g' $<    ; \
	sed -i 's/x_[0-9]\+_//g' $<        ; \
	sed '18!d' $<                 > $@ ; \
	echo "        ..."           >> $@ ; \
	sed '54,60!d' $<             >> $@ ; \
	echo "..."           >> $@

$(LLVM_GRIN_FILES): $(LLVM_GRIN_FILES_DEP)
	cat $^ > $@

$(LLVM_CODE_FILES): $(LLVM_CODE_FILES_DEP)
	cat $^ > $@

$(LLVM_SILLY_FILES): $(LLVM_SILLY_FILES_DEP)
	sed '33,35!d' $<        > $@ ; \
	echo "        ..."     >> $@ ; \
	sed '63,77!d' $<       >> $@ ; \
	echo "        ..."     >> $@ ; \
	echo "}"               >> $@ ; \
	sed -i 's/fun_//' $@         ; \
	sed -i 's/, )/)/' $@         ; \
	sed -i 's/ )/)/' $@          ; \
	sed -i 's/ ;/;/' $@          ; \
