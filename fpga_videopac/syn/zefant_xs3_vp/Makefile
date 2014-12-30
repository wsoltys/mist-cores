##############################################################################
#
# Generic Makefile for ISE projects
#
# $Id: Makefile,v 1.8 2007/06/09 11:11:37 arnim Exp $
#
# Execute with 'make <target>', where target is one of
#   ise           : build ISE project from
#                   - files in compile_list
#                   - project settings in timing.ucf and <board>.ucf
#                   - project settings applied in gen_ise_project.tcl
#   compile       : compile ISE project by means of tcl script
#                   compile_project.tcl
#   <blank> | all : run through all targets above
#
##############################################################################

export MODULE := zefant_xs3_vp

export COMPILE_LIST := compile_list
SOURCE_FILES := $(shell cat $(COMPILE_LIST))
export BLD := bld

.PHONY: all
all: $(BLD)/$(MODULE).svf

.PHONY: ise
ise: $(BLD)/$(MODULE).ise

.PHONY: compile
compile: $(BLD)/$(MODULE).bit

../../roms/hex/rom_t48.vhd: ../../roms/bios/o2rom.bin
	cd ../../roms/hex; \
	make rom_t48.vhd

$(BLD)/$(MODULE).ise: $(COMPILE_LIST) *.ucf gen_ise_project.tcl ../../roms/hex/rom_t48.vhd
	xtclsh gen_ise_project.tcl

$(BLD)/$(MODULE).bit: $(SOURCE_FILES) ac97_defines.v $(BLD)/$(MODULE).ise compile_project.tcl
	xtclsh compile_project.tcl
	touch $@

$(BLD)/$(MODULE).svf: $(BLD)/$(MODULE).bit
	./gen_svf.sh

.PHONY: clean
clean:
	rm -rf *~ bld
	cd ../../roms/hex; \
	make clean
