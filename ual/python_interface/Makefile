CPODEF ?= CPODef.xml
INSTALL_DIR ?= ${PWD}/python_pk

all : setup.py
	python setup.py build

clean:
	rm -rf build *~ $(INSTALL_DIR)/ual/*

clean-src: clean
	rm -f *.py *.pyc 
	rm -rf python_pk

install: all
	mkdir -p $(INSTALL_DIR)/ual
	cp ascii_cpo/ascii_cpo.py $(INSTALL_DIR)/.
	python setup.py install --install-lib=$(INSTALL_DIR)/ual
	@echo "===================================================="; \
	echo "Add ${INSTALL_DIR} to your PYTHONPATH"

setup.py: CPODef2PyItm.xsl CPODef2PyRoutines.xsl 
	@cp lowlevel/ual_low_level_wrapper.py .
	xsltproc CPODef2PyItm.xsl $(CPODEF) 
	xsltproc CPODef2PyRoutines.xsl $(CPODEF) 


