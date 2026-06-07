from pathlib import Path

from py65.devices.mpu65c02 import MPU as CMOS65C02

HERE = Path(__file__).parent
ROM_PATH = HERE.parent / "forth-emu.bin"
LBL_PATH = HERE.parent / "forth-emu.lbl"
ROM_ADDR = 0x8000
TRAP_ADDR = 0x0300
THREAD_ADDR = 0x0310
IP_ADDR = 0xFC
DTOP_VALUE = 0xF4


class ForthTestVM:
    def __init__(self):
        self.symbols = self._parse_lbl(LBL_PATH)
        self.mpu = CMOS65C02()
        self.mpu.memory = [0xEA] * 0x10000
        with open(ROM_PATH, "rb") as f:
            program = f.read()
        self.mpu.memory[ROM_ADDR : ROM_ADDR + len(program)] = list(program)
        self._init_forth()

    @staticmethod
    def _parse_lbl(path):
        symbols = {}
        with open(path) as f:
            for line in f:
                parts = line.strip().split()
                if len(parts) >= 3 and parts[0] == "al":
                    addr = int(parts[1], 16)
                    name = parts[2].lstrip(".")
                    symbols[name] = addr
        return symbols

    def _init_forth(self):
        self.mpu.pc = self._get_word(self.mpu.RESET)
        entry_point = self.symbols["entry_point"]
        steps = 0
        while self.mpu.pc != entry_point and steps < 10000:
            self.mpu.step()
            steps += 1
        assert self.mpu.pc == entry_point, (
            f"Forth init failed to reach entry_point, PC=${self.mpu.pc:04X}"
        )

    def _get_word(self, addr):
        return self.mpu.memory[addr] | (self.mpu.memory[addr + 1] << 8)

    def _set_word(self, addr, value):
        self.mpu.memory[addr] = value & 0xFF
        self.mpu.memory[addr + 1] = (value >> 8) & 0xFF

    def push(self, value):
        self._set_word(self.mpu.x, value & 0xFFFF)
        self.mpu.x = (self.mpu.x - 2) & 0xFF

    def tos(self):
        return self._get_word((self.mpu.x + 2) & 0xFF)

    def nos(self):
        return self._get_word((self.mpu.x + 4) & 0xFF)

    def stack(self, depth=4):
        return [self._get_word((self.mpu.x + 2 + i * 2) & 0xFF) for i in range(depth)]

    def stack_depth(self):
        return (DTOP_VALUE - self.mpu.x) // 2

    def reset_stack(self):
        self.mpu.x = DTOP_VALUE

    def peek(self, addr):
        return self.mpu.memory[addr]

    def poke(self, addr, value):
        self.mpu.memory[addr] = value

    def _setup_trap(self):
        self.mpu.memory[TRAP_ADDR] = 0x4C
        self._set_word(TRAP_ADDR + 1, TRAP_ADDR)

    def _run_until_trap(self, timeout=100000):
        steps = 0
        while self.mpu.pc != TRAP_ADDR and steps < timeout:
            self.mpu.step()
            steps += 1
        if steps >= timeout:
            pc = self.mpu.pc
            depth = self.stack_depth()
            msg = f"Execution timed out at PC=${pc:04X} stack={depth} items"
            raise RuntimeError(msg)

    def execute(self, name):
        word_addr = self.symbols.get(f"do_{name}")
        if word_addr is None:
            word_addr = self.symbols.get(name)
        if word_addr is None:
            msg = f"Unknown word: {name}"
            raise ValueError(msg)

        self._setup_trap()
        self._set_word(THREAD_ADDR, word_addr)
        self._set_word(THREAD_ADDR + 2, TRAP_ADDR)
        self._set_word(IP_ADDR, THREAD_ADDR)
        self.mpu.pc = self.symbols["NEXT"]

        self._run_until_trap()

    def execute_thread(self, cells):
        cells = list(cells) + [TRAP_ADDR]
        self._setup_trap()
        for i, w in enumerate(cells):
            self._set_word(THREAD_ADDR + i * 2, w)
        self._set_word(IP_ADDR, THREAD_ADDR)
        self.mpu.pc = self.symbols["NEXT"]

        self._run_until_trap()

    def set_return_stack(self, *values):
        sp = self.mpu.sp
        for v in values:
            self.mpu.memory[0x100 + sp] = (v >> 8) & 0xFF
            sp = (sp - 1) & 0xFF
            self.mpu.memory[0x100 + sp] = v & 0xFF
            sp = (sp - 1) & 0xFF
        self.mpu.sp = sp

    def lookup(self, name):
        """Walk the Forth dictionary at runtime to find a word's CFA (XT).

        This works for any word in the dictionary, including those defined
        in bootstrap.f (which have no symbol table entries).
        """
        latest_addr = self.symbols.get("LATEST", 0x0200)
        header = self._get_word(latest_addr)
        while header != 0:
            ln_byte = self.mpu.memory[header + 2]
            if ln_byte & 0x40:
                header = self._get_word(header)
                continue
            name_len = ln_byte & 0x1F
            raw = bytes(
                self.mpu.memory[header + 3 : header + 3 + name_len]
            )
            stored = raw.decode("ascii", errors="replace")
            if stored == name:
                return header + 3 + name_len
            header = self._get_word(header)
        raise ValueError(f"Word not found in dictionary: {name}")

    def execute_raw(self, raw_bytes):
        """Write raw bytes to thread area and execute (for CLIT, LITSTR etc)."""
        self._setup_trap()
        for i, b in enumerate(raw_bytes):
            self.mpu.memory[THREAD_ADDR + i] = b & 0xFF
        self._set_word(IP_ADDR, THREAD_ADDR)
        self.mpu.pc = self.symbols["NEXT"]
        self._run_until_trap()
