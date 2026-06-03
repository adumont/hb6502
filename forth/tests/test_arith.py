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


def test_2PLUS(vm):
    vm.push(0x0001)
    vm.execute('2PLUS')
    assert vm.tos() == 0x0003


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


def test_EQZ_zero(vm):
    vm.push(0x0000)
    vm.execute('EQZ')
    assert vm.tos() == 0xFFFF


def test_EQZ_nonzero(vm):
    vm.push(0x0001)
    vm.execute('EQZ')
    assert vm.tos() == 0x0000
