# build tools & options
CL65 = cl65
CLFLAGS  = -v -d -t none -O --cpu 65c02 -C $(LIB)/sbc.cfg\
  -m  $(basename $@).map\
  -Ln $(basename $@).lbl

LIB=lib

.DEFAULT_GOAL := run

### STAGE 1

# Builds forth-stage1.bin
%-stage1.bin: %.s
	$(CL65) $(CLFLAGS) -o $@ $<

# Builds the 3 .dat files, cross-compiling the bootstrap.f code
%.dat: forth-stage1.bin bootstrap.f
	./xcompiler.py -r forth-stage1.bin -l bootstrap.f -s forth-stage1.lbl

stage1: rom.dat ram.dat last.dat

### STAGE 2

# At the moment, LINKING <=> STAGE2
# TODO: replace with STAGE1 flag and invert logic (ifdef => ifndef)

## For the Emulator

# Builds the emulator image, using LINKING flag!
forth-emu.bin: forth.s rom.dat ram.dat last.dat
	$(CL65) $(CLFLAGS) --asm-define LINKING -o $@ $<

emu: forth-emu.bin

run: forth-emu.bin
	./py65forth.py -r $<

debug: forth-emu.bin
	./debugger.py -r $<

## For the HW 6502 computer

# Builds the hw image (using ACIA flag!)
forth-hw.bin: forth.s rom.dat ram.dat last.dat
	$(CL65) $(CLFLAGS) --asm-define ACIA --asm-define LINKING -o $@ $<

hw: forth-hw.bin

flash: forth-hw.bin
	minipro -p AT28C256 -w $<

## HELP

help:
	@echo "usage: make [run | flash]"
	@echo ""
	@echo "  run:    build and execute the rom in 6502 emulator"
	@echo "  flash:  build and flash the rom for 6502 hw computer"

clean:
	-rm -f lib/*.o *.o *.hex *.map *.bin *.h *.lbl *.dat
