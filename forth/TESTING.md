# Testing AlexFORTH with pytest + py65

Tests execute actual 65C02 Forth assembly via the
[py65](https://github.com/mnaberez/py65) emulator.

## Quickstart

```sh
cd forth
make test          # builds forth-emu.bin then runs pytest
# or skip rebuild:
uv run python -m pytest tests/
```

## How it works — `ForthTestVM`

The `vm` fixture (in `conftest.py`) creates a `ForthTestVM` which:

1. Loads `forth-emu.bin` into a py65 `MPU` at `$8000`
2. Steps the CPU from RESET to `entry_point` (end of RAM test / init)
3. Resets the data stack (X register = `$F4`)

All constants in `helpers.py`:

| Constant      | Value   | Meaning                           |
| ------------- | ------- | --------------------------------- |
| `ROM_ADDR`    | `$8000` | ROM load address                  |
| `TRAP_ADDR`   | `$0300` | Trap: `JMP $0300` stops execution |
| `THREAD_ADDR` | `$0310` | Scratch area for test threads     |
| `IP_ADDR`     | `$FC`   | Forth IP variable (2 bytes in ZP) |
| `DTOP_VALUE`  | `$F4`   | Initial data stack pointer (in X) |

### Reading the symbol table

The `.lbl` file is parsed at startup.  Look up word addresses:

```python
vm.symbols['do_DUP']     # → $8E9F
vm.symbols['do_PLUS']    # → $8E0E
vm.symbols['entry_point'] # → $8071
vm.symbols['NEXT']       # → $807D
```

## Writing tests — two patterns

### Pattern A: simple primitive (easiest)

Push operands, call `execute('NAME')`, assert stack state.

```python
def test_DUP(vm):
    vm.push(0x1234)
    vm.execute('DUP')
    assert vm.stack_depth() == 2
    assert vm.tos() == 0x1234
    assert vm.nos() == 0x1234
```

`execute()` looks up `do_DUP` in the symbol table, sets up a 2-cell thread
(`[do_DUP, TRAP_ADDR]`), and runs until PC hits `$0300`.

### Pattern B: multi-word thread

For words that need operands in the thread itself (JUMP, 0BR, *DO, *LOOP):

```python
def test_0BR_branch_when_zero(vm):
    vm.push(0)                               # TOS = 0 → take branch
    cells = [vm.symbols['do_0BR'], THREAD_ADDR + 4]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 0
```

`execute_thread()` appends `TRAP_ADDR` as the last cell, so execution always
terminates by hitting the trap.

**Loop-back targets:** point to the cell address in the thread:

```python
cells = [do_STAR_DO, do_I, do_STAR_LOOP, THREAD_ADDR + 2]
#                                           ^^^^ loop back to do_I
```

`THREAD_ADDR + 2` = offset of `do_I` in the thread (2 bytes per cell).

### Pattern C: return stack setup

For I, J, LEAVE — words that read the 6502 hardware stack:

```python
def test_I_returns_index_from_rs(vm):
    vm.set_return_stack(100, 42)   # END=100 deep, I=42 on top
    vm.execute('I')                # or execute_thread([do_I])
    assert vm.tos() == 42
```

`set_return_stack()` pushes 16-bit values HI-byte-first (matching `PHA` order).
First arg ends up deepest on the stack — so `(END, I)` matches *DO's layout.

## Helper API

| Method                           | Purpose                         |
| -------------------------------- | ------------------------------- |
| `push(value)`                    | Push 16-bit to data stack       |
| `tos()`                          | Top of stack (value at `2,X`)   |
| `nos()`                          | Next on stack (value at `4,X`)  |
| `stack(depth)`                   | List of stack values, TOS first |
| `stack_depth()`                  | Number of items on data stack   |
| `reset_stack()`                  | X = `DTOP_VALUE`                |
| `peek(addr)` / `poke(addr, val)` | Read/write single byte          |
| `execute('NAME')`                | Run a single Forth word         |
| `execute_thread(cells)`          | Run a multi-cell thread         |
| `set_return_stack(*values)`      | Set up hardware (return) stack  |

## Memory map used by tests

| Range         | Usage                                                 |
| ------------- | ----------------------------------------------------- |
| `$0300`       | Trap code: `JMP $0300`                                |
| `$0310+`      | Test thread (written by `execute` / `execute_thread`) |
| `$00FC-$00FD` | Forth IP variable                                     |
| `$0400`       | Scratch RAM (FETCH/STORE tests)                       |

Do not use `$0300` or `$0310-$031F` for scratch data — they conflict with
the test harness.

## Adding a new test file

```sh
touch tests/test_myfeature.py
# write tests using the `vm` fixture
uv run python -m pytest tests/test_myfeature.py -v
```

The `vm` fixture is auto-discovered from `conftest.py`.  No registration needed.

## Known constraints

- **TRUE = $FFFF** (not $0001) — `EQZ` returns `$FFFF` for zero
- **Stack model:** TOS at `2,X`, X points to the *free slot above* TOS
  (`DTOP_VALUE`).  Push writes to `0,X` then DEX.
- **JUMP** reads an *absolute* 16-bit address from the thread, not a relative
  offset.  It cannot jump directly to machine code — only to another thread
  cell.
- **Colon words** (`:`, `;`) use `do_COLON` and `do_SEMI` which manipulate the
  return stack.  Testing them directly requires careful return-stack setup.
- **Trap mechanism:** `NEXT` reads the next cell from `(IP)` and `JMP (W)` to
  it.  The trap cell has value `$0300`, so NEXT jumps to the JMP instruction
  at `$0300`, which loops on itself.  The test loop checks `PC == $0300`.

## todo / future test ideas

```
test_compiler.py  — : ; LITERAL COMPILE [ ] IMMEDIATE
test_bootstrap.py — end-to-end cross-compilation verification
edge cases        — underflow, overflow, boundary values (0, $FFFF)
performance       — cycle-count measurements from mpu.clock
```
