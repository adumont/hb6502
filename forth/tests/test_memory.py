def test_FETCH(vm):
    addr = 0x0400
    vm.poke(addr, 0x34)
    vm.poke(addr + 1, 0x12)
    vm.push(addr)
    vm.execute('FETCH')
    assert vm.tos() == 0x1234


def test_STORE(vm):
    addr = 0x0400
    vm.push(0xABCD)
    vm.push(addr)
    vm.execute('STORE')
    assert vm.peek(addr) == 0xCD
    assert vm.peek(addr + 1) == 0xAB


def test_CFETCH(vm):
    vm.poke(0x0400, 0x42)
    vm.push(0x0400)
    vm.execute('CFETCH')
    assert vm.tos() == 0x0042


def test_CSTORE(vm):
    vm.push(0x0042)
    vm.push(0x0400)
    vm.execute('CSTORE')
    assert vm.peek(0x0400) == 0x42
