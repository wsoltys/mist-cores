
MODULE = menu

ASL_OPTS = -i ../include -L

.PHONY: all
all: $(MODULE)_o2em.bin rom.bin

$(MODULE).p: $(MODULE).asm carts.asm
	asl $(ASL_OPTS) -o $@ $<
$(MODULE)_o2em.p: $(MODULE).asm carts_o2em.asm
	asl -D FOR_O2EM $(ASL_OPTS) -o $@ $<

$(MODULE)_o2em.bin: $(MODULE)_o2em.p
	p2bin $< $@ -r 1024-3071

carts.asm: gen_carts.pl
	./gen_carts.pl > $@


$(MODULE).bin: $(MODULE).p
	p2bin $< $@ -r 1024-7167

rom.bin: $(MODULE).bin carts.bin
	cat $(MODULE).bin carts.bin > $@


.PHONY: clean
clean:
	rm -rf *~ *.p *.bin *.lst carts.asm
