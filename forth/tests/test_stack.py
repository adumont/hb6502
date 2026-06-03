def test_DUP(vm):
    vm.push(0x1234)
    vm.execute('DUP')
    assert vm.stack_depth() == 2
    assert vm.tos() == 0x1234
    assert vm.nos() == 0x1234


def test_DROP(vm):
    vm.push(0x1111)
    vm.push(0x2222)
    vm.execute('DROP')
    assert vm.stack_depth() == 1
    assert vm.tos() == 0x1111


def test_SWAP(vm):
    vm.push(0x1234)
    vm.push(0xABCD)
    vm.execute('SWAP')
    assert vm.stack_depth() == 2
    assert vm.tos() == 0x1234
    assert vm.nos() == 0xABCD


def test_OVER(vm):
    vm.push(0x1111)
    vm.push(0x2222)
    vm.execute('OVER')
    assert vm.stack_depth() == 3
    assert vm.tos() == 0x1111
    assert vm.nos() == 0x2222
    assert vm.stack(3)[2] == 0x1111


def test_ROT(vm):
    vm.push(0x1111)
    vm.push(0x2222)
    vm.push(0x3333)
    vm.execute('ROT')
    assert vm.stack_depth() == 3
    assert vm.tos() == 0x1111
    assert vm.nos() == 0x3333
    assert vm.stack(3)[2] == 0x2222


def test_NROT(vm):
    vm.push(0x1111)
    vm.push(0x2222)
    vm.push(0x3333)
    vm.execute('NROT')
    assert vm.stack_depth() == 3
    assert vm.tos() == 0x2222
    assert vm.nos() == 0x1111
    assert vm.stack(3)[2] == 0x3333


def test_2DROP(vm):
    vm.push(0x1111)
    vm.push(0x2222)
    vm.push(0x3333)
    vm.execute('2DROP')
    assert vm.stack_depth() == 1
    assert vm.tos() == 0x1111


def test_2DUP(vm):
    vm.push(0x1111)
    vm.push(0x2222)
    vm.execute('2DUP')
    assert vm.stack_depth() == 4
    assert vm.tos() == 0x2222
    assert vm.nos() == 0x1111
    assert vm.stack(4)[2] == 0x2222
    assert vm.stack(4)[3] == 0x1111


def test_QDUP_returns(vm):
    vm.push(0x0001)
    vm.execute('QDUP')
    assert vm.stack_depth() == 2


def test_QDUP_zero(vm):
    vm.push(0x0000)
    vm.execute('QDUP')
    assert vm.stack_depth() == 1


def test_DUP_deep(vm):
    vm.push(0xAAAA)
    vm.push(0xBBBB)
    vm.execute('DUP')
    assert vm.stack_depth() == 3
    assert vm.tos() == 0xBBBB
    assert vm.nos() == 0xBBBB
    assert vm.stack(3)[2] == 0xAAAA


def test_SWAP_same_value(vm):
    vm.push(0x1234)
    vm.push(0x1234)
    vm.execute('SWAP')
    assert vm.stack_depth() == 2
    assert vm.tos() == 0x1234
    assert vm.nos() == 0x1234


def test_OVER_empty_below(vm):
    vm.push(0xABCD)
    vm.push(0x1234)
    vm.execute('OVER')
    assert vm.stack_depth() == 3
    assert vm.tos() == 0xABCD
    assert vm.nos() == 0x1234
    assert vm.stack(3)[2] == 0xABCD



