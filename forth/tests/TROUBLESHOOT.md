# Test Failure Troubleshooting

## Timeout at PC=$8EC4 stack=0

**Symptom:** `RuntimeError: Execution timed out at PC=$8EC4 stack=0 items`

PC=$8EC4 is the Forth `getc` input-wait loop (`LDA $F004` / `BEQ $8EC4` / `RTS`). The test thread fell through into the Forth interactive interpreter instead of hitting `TRAP_ADDR` ($0300).

**Root cause (SKI tests):** Dictionary growth during SKI combinator reduction overwrote unexecuted thread cells, corrupting the instruction stream.

### How SKI reduces at runtime

SKI combinators use `:FUNC` (see `forth/ski.f:16`) to dynamically compile new words into the Forth dictionary during `)` application. Each `:FUNC` call writes to the dictionary pointer (DP, zero-page $F6), growing it upward. Complex Church-numeral arithmetic can compile hundreds of bytes:

| Operation | DP growth (from $0316) |
|-----------|----------------------|
| NOT(T) | 11B |
| NOT(F) | 0B |
| CN TWO THREE ))) | 395B |

If DP reaches the thread area during execution, the remaining (unread) thread cells get overwritten.

### Fix: thread addr computed from `end_ram_image` (`helpers.py:24-25`)

```python
rom_end = self.symbols.get("end_ram_image", ROM_ADDR)
self.thread_addr = rom_end + 8
```

The thread is placed right after `end_ram_image` ($A3D3), past all loaded ROM data, safely away from the dictionary (which lives in $0200-$3FFF).

### Diagnostic steps

```python
from tests.helpers import ForthTestVM

vm = ForthTestVM()
dp_before = vm._get_word(0xF6)   # DP_ADDR
print(f"DP before: ${dp_before:04X}")
print(f"thread_addr: ${vm.thread_addr:04X}")

# Run the failing test code...
vm.push(vm.symbols["do_FALSE"])
vm.push(vm.symbols["do_TRUE"])
vm.execute_thread([vm.lookup("T"), ...])

dp_after = vm._get_word(0xF6)
print(f"DP after: ${dp_after:04X}")
print(f"Growth: {dp_after - dp_before}B")
```

### Memory map

```
$0200-$0315  Forth dictionary headers
$0316+       DP grows up (dictionary + :FUNC compilations)
...          (free RAM)
$3FFF        ← top of RAM segment
-----
$4000-$7FFF  Unmapped (all $EA in emulator)
-----
$8000        ← ROM_ADDR (forth-emu.bin loaded here)
...          ROM code and data
$A3D3        ← end_ram_image
$A3DB        ← THREAD_ADDR (end_ram_image + 8)
...          padding zeros
$FFFF        end of 16-bit address space
```

### Other timeout causes

- **Stack corruption:** If the data stack underflows or overflows during thread execution, the inner interpreter may read garbage.
- **Trap not at end of thread:** Every thread must end with `TRAP_ADDR` as its last cell (appended automatically by `execute_thread`).
