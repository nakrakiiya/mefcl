all: XSB-TAGS CIAO-TAGS SWI-TAGS MERCURY-TAGS ECLIPSE-TAGS

# XSB 3.3.7

XSB_ROOT_DIR=~/xsb/3.3.7

XSB-TAGS:
	find $(XSB_ROOT_DIR)/packages -type f -name '*.P' | xargs etags -l prolog -o $@

# Ciao 1.14
CIAO_ROOT_DIR=~/CiaoDE

CIAO-TAGS:
	touch $@
	find $(CIAO_ROOT_DIR)/lib/ciao/ciao-1.14/library -type f -name '*.pl' | xargs etags -a -l prolog -o $@
	find $(CIAO_ROOT_DIR)/lib/ciao/ciao-1.14/contrib -type f -name '*.pl' | xargs etags -a -l prolog -o $@
	find $(CIAO_ROOT_DIR)/lib/ciao/ciao-1.14/lib -type f -name '*.pl' | xargs etags -a -l prolog -o $@

# SWI-PROLOG 6.0.2

SWI_ROOT_DIR=~/swi

SWI-TAGS:
	touch $@
	find $(SWI_ROOT_DIR)/lib/swipl-6.0.2/boot -type f -name '*.pl' | xargs etags -a -l prolog -o $@
	find $(SWI_ROOT_DIR)/lib/swipl-6.0.2/library -type f -name '*.pl' | xargs etags -a -l prolog -o $@
	find $(SWI_ROOT_DIR)/lib/swipl-6.0.2/xpce -type f -name '*.pl' | xargs etags -a -l prolog -o $@

# Mercury 11.07.2

MERCURY_SOURCE_DIR=~/source/mercury-compiler-11.07.2

MERCURY-TAGS:
	touch $@
	find $(MERCURY_SOURCE_DIR)/ -type f -name '*.m' | xargs mtags -e
	mv TAGS $@

# ECLiPSe 6.0.199

ECLIPSE_ROOT_DIR=~/Eclipse_6.0_199

ECLIPSE-TAGS:
	touch $@
	find $(ECLIPSE_ROOT_DIR)/lib -type f -name '*.pl' -o -name '*.ecl' | xargs etags -a -l prolog -o $@
	find $(ECLIPSE_ROOT_DIR)/lib_tcl -type f -name '*.pl' -o -name '*.ecl' | xargs etags -a -l prolog -o $@
	find $(ECLIPSE_ROOT_DIR)/lib_public -type f -name '*.pl' -o -name '*.ecl' | xargs etags -a -l prolog -o $@


clean:
	rm -fv *TAGS *~

.PHONY: clean
