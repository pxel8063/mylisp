## -*- mode: makefile-gmake -*-

# set up environment
 include mk/default.mk	# defaults, customizable via "local.mk"
-include local.mk	# optional local customization, use default.mk as template

all: mylisp.elc

%.elc: %.el
	@$(info Compiling file $<)
	@$(EMACSQ) --batch -f batch-byte-compile $<


.PHONY: clean
clean:
	$(RM) mylisp.elc
