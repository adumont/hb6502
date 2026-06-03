def test_HERE(vm):
    vm.execute('HERE')
    assert vm.stack_depth() == 1
    addr = vm.tos()
    assert addr >= 0x0200


def test_HEREPP(vm):
    vm.execute('HERE')
    here1 = vm.tos()
    vm.reset_stack()
    vm.execute('HEREPP')
    assert vm.stack_depth() == 0
    vm.execute('HERE')
    here2 = vm.tos()
    assert here2 == here1 + 2


def test_DP(vm):
    vm.execute('DP')
    assert vm.stack_depth() == 1


def test_DP_STORE(vm):
    vm.execute('HERE')
    old = vm.tos()
    vm.reset_stack()
    vm.push(0x0300)
    vm.execute('DP_STORE')
    vm.reset_stack()
    vm.execute('HERE')
    assert vm.tos() == 0x0300
    vm.reset_stack()
    vm.push(old)
    vm.execute('DP_STORE')


def test_ALLOT(vm):
    vm.execute('HERE')
    before = vm.tos()
    vm.reset_stack()
    vm.push(10)
    vm.execute('ALLOT')
    vm.reset_stack()
    vm.execute('HERE')
    assert vm.tos() == before + 10


def test_ALLOT_negative(vm):
    vm.execute('HERE')
    before = vm.tos()
    vm.reset_stack()
    vm.push(-4 & 0xFFFF)
    vm.execute('ALLOT')
    vm.reset_stack()
    vm.execute('HERE')
    assert vm.tos() == before - 4


def test_COMMAv3(vm):
    vm.execute('HERE')
    addr = vm.tos()
    vm.reset_stack()
    vm.push(0xABCD)
    vm.execute('COMMA')
    addr_val = vm.peek(addr) | (vm.peek(addr + 1) << 8)
    assert addr_val == 0xABCD


def test_COMMAv3_advances_here(vm):
    vm.execute('HERE')
    before = vm.tos()
    vm.reset_stack()
    vm.push(0x1234)
    vm.execute('COMMA')
    vm.reset_stack()
    vm.execute('HERE')
    assert vm.tos() == before + 2


def test_CCOMMA(vm):
    vm.execute('HERE')
    addr = vm.tos()
    vm.reset_stack()
    vm.push(0x0042)
    vm.execute('CCOMMA')
    assert vm.peek(addr) == 0x42


def test_CCOMMA_advances_here(vm):
    vm.execute('HERE')
    before = vm.tos()
    vm.reset_stack()
    vm.push(0x0077)
    vm.execute('CCOMMA')
    vm.reset_stack()
    vm.execute('HERE')
    assert vm.tos() == before + 1


def test_CMOVE(vm):
    for i in range(4):
        vm.poke(0x0200 + i, 0xAA + i)
    vm.push(0x0200)
    vm.push(0x0300)
    vm.push(4)
    vm.execute('CMOVE')
    for i in range(4):
        assert vm.peek(0x0300 + i) == 0xAA + i


def test_LATEST(vm):
    vm.execute('LATEST')
    assert vm.stack_depth() == 1


def test_LAST(vm):
    vm.execute('LAST')
    assert vm.stack_depth() == 1


def test_CFA(vm):
    vm.execute('CFA')
    assert vm.stack_depth() == 0


def test_TO_ROM_changes_here(vm):
    vm.execute('HERE')
    ram_here = vm.tos()
    vm.reset_stack()
    vm.execute('TO_ROM')
    vm.reset_stack()
    vm.execute('HERE')
    rom_here = vm.tos()
    assert rom_here != ram_here


def test_TO_ROM_TO_RAM_roundtrip(vm):
    vm.execute('HERE')
    original_here = vm.tos()
    vm.reset_stack()
    vm.execute('TO_ROM')
    vm.reset_stack()
    vm.execute('TO_RAM')
    vm.reset_stack()
    vm.execute('HERE')
    assert vm.tos() == original_here


def test_TO_ROM_idempotent(vm):
    vm.execute('HERE')
    ram_here = vm.tos()
    vm.reset_stack()
    vm.execute('TO_ROM')
    vm.reset_stack()
    vm.execute('TO_ROM')
    vm.reset_stack()
    vm.execute('HERE')
    rom_here = vm.tos()
    vm.reset_stack()
    vm.execute('TO_RAM')
    assert rom_here != ram_here


def test_TO_RAM_preserves_here(vm):
    vm.execute('HERE')
    before = vm.tos()
    vm.reset_stack()
    vm.execute('TO_ROM')
    vm.reset_stack()
    vm.execute('TO_RAM')
    vm.reset_stack()
    vm.execute('HERE')
    assert vm.tos() == before
