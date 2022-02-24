# build tools & options
CL65 = cl65
CLFLAGS  = -v -d -t none -O --cpu 65c02 -C $(LIB)/sbc.cfg\
  -m  $(basename $@).map\
  -Ln $(basename $@).lbl

HEXDUMP = hexdump
HEXDUMP_ARGS = -v -e '1/1 "%02x " "\n"'
XXD = xxd
XXD_ARGS = -i
LIB=lib

ifdef ACIA
  OPT:=$(OPT) --asm-define ACIA
endif

ifdef LINKING
  OPT:=$(OPT) --asm-define LINKING
endif

.DEFAULT_GOAL := run

### STAGE 1

# Builds forth-stage1.bin
%-stage1.bin: %.s
	$(CL65) $(CLFLAGS) -o $@ $<

# Builds the 3 .dat files, cross-compiling th bootstrap.f code
%.dat: forth-stage1.bin bootstrap.f
	./xcompiler.py -r forth-stage1.bin -l bootstrap.f

### STAGE 2

# At the moment, LINKING <=> STAGE2
# TODO: replace with STAGE1 flag and invert logic (ifdef => ifndef)

# Builds the emulator image, using LINKING flag!
forth-emu.bin: forth.s rom.dat ram.dat last.dat
	$(CL65) $(CLFLAGS) --asm-define LINKING -o $@ $<

# Builds the hw image (using ACIA flag!)
forth-hw.bin: forth.s rom.dat ram.dat last.dat
	$(CL65) $(CLFLAGS) --asm-define ACIA --asm-define LINKING -o $@ $<

run: forth-emu.bin
	./py65forth.py -r $<

flash: forth-hw.bin
	minipro -p AT28C256 -w $<

clean:
	-rm -f lib/*.o *.o *.hex *.map *.bin *.h *.lbl *.dat