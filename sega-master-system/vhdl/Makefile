TARGETS := sms_vga sms_tv sms_megawing

MEM_FILE:=../bootloader/all.mem
DUT      := xc6slx9tqg144-3
SRCS     := $(wildcard src/*.vhdl)

load: sms_vga_final.bit
	sudo papilio-prog -v -f $<

all: $(TARGETS:%=%_final.bit)

.PHONY: load all clean


#-----------------------------------------

clean:
	rm -I $(TARGETS:%=%.prj) $(TARGETS:%=%.xst) $(TARGETS:%=%.ngc) $(TARGETS:%=%.ngd) $(TARGETS:%=%._map.ncd) $(TARGETS:%=%.pcf) $(TARGETS:%=%.ncd) $(TARGETS:%=%.bit) $(TARGETS:%=%.twr)

#-----------------------------------------

# remove Make's built-in rules
.SUFFIXES:

# Tools can run in different contextual/verbosity modes:
# standalone (empty string) or "-intstyle ise|xflow|silent"
MODE  := 

.SECONDARY: $(TARGETS:%=%.ngc) $(TARGETS:%=%.ngd) $(TARGETS:%=%.ncd) $(TARGETS:%=%.bit)

%.prj: sms_xxx.prj.base
	sed s/%/$*/g sms_xxx.prj.base > $*.prj

%.xst: sms_xxx.xst.base
	sed s/%/$*/g sms_xxx.xst.base > $*.xst

%.ngc: %.xst %.prj $(SRCS)
	xst $(MODE) -ifn $*.xst
	@if test ! -f $*.ngc; then exit 2; fi

%.ngd: %.ngc src/%.ucf
	ngdbuild $(MODE) -p $(DUT) -uc src/$*.ucf -bm src/sms.bmm $*.ngc $*.ngd

%_map.ncd %.pcf: %.ngd
	map $(MODE) -w $*.ngd -p $(DUT) -o $*_map.ncd $*.pcf

%.ncd: %_map.ncd %.pcf
	par $(MODE) -w $*_map.ncd $*.ncd $*.pcf

%.bit: %.ncd
	bitgen $(MODE) -w $*.ncd

%.twr: %.ncd %.pcf
	trce $(MODE) $*.ncd $*.pcf -xml $*.twx -o $*.twr $(TIM_OPT)

%_final.bit: %.bit src/sms_bd.bmm $(MEM_FILE)
	data2mem -bm src/sms_bd.bmm -bd $(MEM_FILE) -bt $*.bit -o b $*_final.bit

