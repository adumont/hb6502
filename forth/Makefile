# build tools & options
CL65 = cl65
CLFLAGS  = -v -d -t none -O --cpu 65c02 -C $(LIB)/sbc.cfg -m $*.map
HEXDUMP = hexdump
HEXDUMP_ARGS = -v -e '1/1 "%02x " "\n"'
XXD = xxd
XXD_ARGS = -i
LIB=lib

ifdef ACIA
  OPT:= --asm-define ACIA
endif

%.hex: %.bin
	$(HEXDUMP) $(HEXDUMP_ARGS) $< > $@

%.bin: %.s #$(LIB)/interrupt.s $(LIB)/vectors.s $(LIB)/copydata.s
	$(CL65) $(CLFLAGS) $(OPT) -Ln $*.lbl -o $@ $^
	@echo Flash with: ../programmer/eeprom.py flash $@

%.h: %.bin
	#cp $< /tmp/rom.bin
	#( cd /tmp; $(XXD) $(XXD_ARGS) rom.bin ) > $@
	xxd -i $< | sed -e "s/unsigned char $*_bin\[\]/const byte rom_bin\[\] PROGMEM/" > $@
	cp $@ ../mega/rom.h

clean:
	-rm -f lib/*.o *.o *.hex *.map *.bin *.h *.lbl
