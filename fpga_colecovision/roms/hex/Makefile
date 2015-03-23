
FILTER_HEX = egrep '^:......0[01]'

.PHONY: all
all: vdp18_ram_fill.hex coleco.hex cart_l.hex cart_u.hex coleco_bios.vhd

vdp18_ram_fill.hex: ../test/vdp18_ram_fill.bin
	srec_cat ../test/vdp18_ram_fill.bin -binary -unfill 0x00 -o - -intel | $(FILTER_HEX) > vdp18_ram_fill.hex

coleco.hex: ../bios/coleco.bin
	srec_cat ../bios/coleco.bin -binary  -o - -intel | $(FILTER_HEX) > coleco.hex

coleco_bios.vhd: ../bios/coleco.bin
	hex2rom -b ../bios/coleco.bin coleco_bios 13l8s > coleco_bios.vhd

cart_l.hex: ../carts/cart.bin
	srec_cat -o - -Intel ../carts/cart.bin -Binary -Split 2 0 | $(FILTER_HEX) > cart_l.hex
cart_u.hex: ../carts/cart.bin
	srec_cat -o - -Intel ../carts/cart.bin -Binary -Split 2 1 | $(FILTER_HEX) > cart_u.hex



.PHONY: clean
clean:
	rm -f *hex *~ *vhd
