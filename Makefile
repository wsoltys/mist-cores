VERSION = 0.1
NAME = apple2fpga-$(VERSION)

VHDL_SRC = timing_generator.vhd \
	character_rom.vhd \
	video_generator.vhd \
	main_roms.vhd \
	cpu6502.vhd \
	apple2.vhd \
	vga_controller.vhd \
	PS2_Ctrl.vhd \
	kbd_transl.vhd \
	kbd_intf.vhd \
	disk_ii_rom.vhd \
	disk_ii.vhd \
	spi_controller.vhd \
	DE2_TOP.vhd \
	testbench.vhd

TARFILES = README Makefile dsk2nib.c $(VHDL_SRC) CLK14MPLL.ppf CLK14MPLL.vhd \
	apple2fpga.qpf DE2_TOP.qsf dos33master.dsk rom2vhdl \
	apple_II.rom slot6.rom

testbench : $(VHDL_SRC:%.vhd=%.o)
	ghdl -e testbench

dsk2nib : dsk2nib.c
	cc -O -o dsk2nib dsk2nib.c

apple2fpga-$(VERSION).tar.gz : $(TARFILES)
	mkdir $(NAME)
	ln $(TARFILES) $(NAME)
	tar zcf $@ $(NAME)
	rm -rf $(NAME)

main_roms.vhd : rom2vhdl apple_II.rom
	./rom2vhdl main_roms 13 12287 < apple_II.rom > $@

disk_ii_rom.vhd : rom2vhdl slot6.rom
	./rom2vhdl disk_ii_rom 7 255 < slot6.rom > $@

%.nib : %.dsk dsk2nib
	./dsk2nib $< $@

%.o : %.vhd
	ghdl -a $<

.PHONY : clean

clean :
	rm -f *.o *.log *.vcd
