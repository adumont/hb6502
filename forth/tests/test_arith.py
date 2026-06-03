def test_PLUS(vm):
    vm.push(0x1111)
    vm.push(0x2222)
    vm.execute('PLUS')
    assert vm.stack_depth() == 1
    assert vm.tos() == 0x3333


def test_MINUS(vm):
    vm.push(0x3333)
    vm.push(0x1111)
    vm.execute('MINUS')
    assert vm.stack_depth() == 1
    assert vm.tos() == 0x2222


def test_1PLUS(vm):
    vm.push(0x0001)
    vm.execute('1PLUS')
    assert vm.tos() == 0x0002


def test_1PLUS_carry(vm):
    vm.push(0x00FF)
    vm.execute('1PLUS')
    assert vm.tos() == 0x0100


def test_1PLUS_wraparound(vm):
    vm.push(0xFFFF)
    vm.execute('1PLUS')
    assert vm.tos() == 0x0000


def test_2PLUS(vm):
    vm.push(0x0001)
    vm.execute('2PLUS')
    assert vm.tos() == 0x0003


def test_2PLUS_carry(vm):
    vm.push(0x00FF)
    vm.execute('2PLUS')
    assert vm.tos() == 0x0101


def test_2PLUS_wraparound(vm):
    vm.push(0xFFFF)
    vm.execute('2PLUS')
    assert vm.tos() == 0x0001


def test_AND(vm):
    vm.push(0xFF00)
    vm.push(0x0FF0)
    vm.execute('AND')
    assert vm.tos() == 0x0F00


def test_OR(vm):
    vm.push(0xFF00)
    vm.push(0x00FF)
    vm.execute('OR')
    assert vm.tos() == 0xFFFF


def test_XOR(vm):
    vm.push(0xFF00)
    vm.push(0x0F0F)
    vm.execute('XOR')
    assert vm.tos() == 0xF00F


def test_NOT(vm):
    vm.push(0xAAAA)
    vm.execute('NOT')
    assert vm.tos() == 0x5555


def test_MINUS_borrow_max(vm):
    vm.push(0x0000)
    vm.push(0xFFFF)
    vm.execute('MINUS')
    assert vm.tos() == 0x0001


def test_MINUS_signed_boundary(vm):
    vm.push(0x8000)
    vm.push(0x0001)
    vm.execute('MINUS')
    assert vm.tos() == 0x7FFF


def test_AND_self(vm):
    vm.push(0x1234)
    vm.push(0x1234)
    vm.execute('AND')
    assert vm.tos() == 0x1234


def test_AND_FFFF(vm):
    vm.push(0x1234)
    vm.push(0xFFFF)
    vm.execute('AND')
    assert vm.tos() == 0x1234


def test_OR_FFFF(vm):
    vm.push(0x1234)
    vm.push(0xFFFF)
    vm.execute('OR')
    assert vm.tos() == 0xFFFF


def test_OR_self(vm):
    vm.push(0x1234)
    vm.push(0x1234)
    vm.execute('OR')
    assert vm.tos() == 0x1234


def test_XOR_FFFF(vm):
    vm.push(0x1234)
    vm.push(0xFFFF)
    vm.execute('XOR')
    assert vm.tos() == 0xEDCB


def test_XOR_zero(vm):
    vm.push(0x1234)
    vm.push(0x0000)
    vm.execute('XOR')
    assert vm.tos() == 0x1234


def test_NOT_zero(vm):
    vm.push(0x0000)
    vm.execute('NOT')
    assert vm.tos() == 0xFFFF


def test_NOT_FFFF(vm):
    vm.push(0xFFFF)
    vm.execute('NOT')
    assert vm.tos() == 0x0000


def test_EQZ_zero(vm):
    vm.push(0x0000)
    vm.execute('EQZ')
    assert vm.tos() == 0xFFFF


def test_EQZ_nonzero(vm):
    vm.push(0x0001)
    vm.execute('EQZ')
    assert vm.tos() == 0x0000


def test_EQZ_FFFF(vm):
    vm.push(0xFFFF)
    vm.execute('EQZ')
    assert vm.tos() == 0x0000


def test_PLUS_overflow(vm):
    vm.push(0xFFFF)
    vm.push(0x0001)
    vm.execute('PLUS')
    assert vm.tos() == 0x0000


def test_PLUS_max(vm):
    vm.push(0xFFFF)
    vm.push(0xFFFF)
    vm.execute('PLUS')
    assert vm.tos() == 0xFFFE


def test_MINUS_underflow(vm):
    vm.push(0x0000)
    vm.push(0x0001)
    vm.execute('MINUS')
    assert vm.tos() == 0xFFFF


def test_AND_zero(vm):
    vm.push(0xFFFF)
    vm.push(0x0000)
    vm.execute('AND')
    assert vm.tos() == 0x0000


def test_OR_zero(vm):
    vm.push(0x1234)
    vm.push(0x0000)
    vm.execute('OR')
    assert vm.tos() == 0x1234


def test_XOR_self(vm):
    vm.push(0x1234)
    vm.push(0x1234)
    vm.execute('XOR')
    assert vm.tos() == 0x0000
