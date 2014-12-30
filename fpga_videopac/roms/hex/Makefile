
FILTER_HEX = egrep '^:......0[01]'

.PHONY: all
all: rom_t48.vhd cart.hex
# cart_l.hex cart_u.hex cart.vhd

rom_t48.vhd: ../bios/o2rom.bin
	hex2rom -b $< rom_t48 10l8s > $@

cart.vhd: ../carts/cart.bin
	hex2rom -b $< cart 12l8s > $@

cart_comp.bin: ../carts/cart.bin
	cat $< $< $< $< > $@

cart.hex: cart_comp.bin
	srec_cat $< -binary -crop 0x0000 0x2000 -o - -intel | $(FILTER_HEX) > $@
cart_l.hex: ../carts/cart.bin
	srec_cat -o - -Intel $< -Binary -Split 2 0 | $(FILTER_HEX) > $@
cart_u.hex: ../carts/cart.bin
	srec_cat -o - -Intel $< -Binary -Split 2 1 | $(FILTER_HEX) > $@

.PHONY: clean
clean:
	rm -f *hex *~ *vhd *bin
