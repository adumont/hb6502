def test_TRUE(vm):
    vm.execute("TRUE")
    assert vm.tos() == 0xFFFF


def test_FALSE(vm):
    vm.execute("FALSE")
    assert vm.tos() == 0x0000


def test_BASE(vm):
    vm.execute("BASE")
    assert vm.stack_depth() == 1


def test_HEX(vm):
    vm.push(10)
    vm.execute("HEX")
    vm.reset_stack()
    vm.execute("BASE")
    addr = vm.tos()
    assert vm.peek(addr) == 16


def test_DEC(vm):
    vm.push(16)
    vm.execute("DEC")
    vm.reset_stack()
    vm.execute("BASE")
    addr = vm.tos()
    assert vm.peek(addr) == 10


def test_BIN(vm):
    vm.push(10)
    vm.execute("BIN")
    vm.reset_stack()
    vm.execute("BASE")
    addr = vm.tos()
    assert vm.peek(addr) == 2


def test_OCT(vm):
    vm.push(10)
    vm.execute("OCT")
    vm.reset_stack()
    vm.execute("BASE")
    addr = vm.tos()
    assert vm.peek(addr) == 8
