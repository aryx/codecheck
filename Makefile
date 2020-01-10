PROGS= scheck \

 linter/lib.cma \

    linter/lib.cma \

  linter \

INSTALL_SUBDIRS= \
linter
#------------------------------------------------------------------------------
# scheck targets
#------------------------------------------------------------------------------

scheck: $(LIBS) $(OBJS) main_scheck.cmo
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^
scheck.opt: $(LIBS:.cma=.cmxa) $(OPTOBJS) main_scheck.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $^

