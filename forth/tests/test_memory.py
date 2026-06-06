def test_FETCH(vm):
    addr = 0x0400
    vm.poke(addr, 0x34)
    vm.poke(addr + 1, 0x12)
    vm.push(addr)
    vm.execute("FETCH")
    assert vm.tos() == 0x1234


def test_STORE(vm):
    addr = 0x0400
    vm.push(0xABCD)
    vm.push(addr)
    vm.execute("STORE")
    assert vm.peek(addr) == 0xCD
    assert vm.peek(addr + 1) == 0xAB


def test_CFETCH(vm):
    vm.poke(0x0400, 0x42)
    vm.push(0x0400)
    vm.execute("CFETCH")
    assert vm.tos() == 0x0042


def test_CSTORE(vm):
    vm.push(0x0042)
    vm.push(0x0400)
    vm.execute("CSTORE")
    assert vm.peek(0x0400) == 0x42


def test_FETCH_FFFF(vm):
    vm.poke(0x0400, 0xFF)
    vm.poke(0x0401, 0xFF)
    vm.push(0x0400)
    vm.execute("FETCH")
    assert vm.tos() == 0xFFFF


def test_STORE_zero(vm):
    vm.push(0x0000)
    vm.push(0x0400)
    vm.execute("STORE")
    assert vm.peek(0x0400) == 0x00
    assert vm.peek(0x0401) == 0x00


def test_CFETCH_FF(vm):
    vm.poke(0x0400, 0xFF)
    vm.push(0x0400)
    vm.execute("CFETCH")
    assert vm.tos() == 0x00FF
