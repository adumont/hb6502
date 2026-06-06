def test_PUSH0(vm):
    vm.execute("PUSH0")
    assert vm.stack_depth() == 1
    assert vm.tos() == 0


def test_PUSH1(vm):
    vm.execute("PUSH1")
    assert vm.stack_depth() == 1
    assert vm.tos() == 1


def test_SP_FETCH(vm):
    vm.reset_stack()
    vm.execute("SP_FETCH")
    assert vm.stack_depth() == 1
    assert vm.tos() == 0xF6


def test_SP_STORE(vm):
    vm.push(0x88)
    vm.execute("SP_STORE")
    assert vm.mpu.x == 0x86


def test_RP_FETCH(vm):
    sp_before = vm.mpu.sp
    vm.execute("RP_FETCH")
    assert vm.stack_depth() == 1
    assert vm.tos() == sp_before


def test_RP_STORE(vm):
    vm.push(0xAA)
    vm.execute("RP_STORE")
    assert vm.mpu.sp == 0xAA


def test_TWICE(vm):
    vm.push(0x1234)
    vm.execute("TWICE")
    assert vm.tos() == 0x2468


def test_TWICE_overflow(vm):
    vm.push(0x8001)
    vm.execute("TWICE")
    assert vm.tos() == 0x0002


def test_HALF_positive(vm):
    vm.push(0x0008)
    vm.execute("HALF")
    assert vm.tos() == 0x0004


def test_HALF_negative(vm):
    vm.push(0xFFF0)
    vm.execute("HALF")
    assert vm.tos() == 0xFFF8


def test_HALF_odd_positive(vm):
    vm.push(0x0007)
    vm.execute("HALF")
    assert vm.tos() == 0x0003


def test_UHALF(vm):
    vm.push(0x0008)
    vm.execute("UHALF")
    assert vm.tos() == 0x0004


def test_UHALF_unsigned(vm):
    vm.push(0x8000)
    vm.execute("UHALF")
    assert vm.tos() == 0x4000


def test_DTWICE(vm):
    vm.push(0x1111)
    vm.push(0x2222)
    vm.execute("DTWICE")
    assert vm.stack_depth() == 2
    assert vm.tos() == 0x4444
    assert vm.nos() == 0x2222


def test_DHALF(vm):
    vm.push(0x1111)
    vm.push(0x0008)
    vm.execute("DHALF")
    assert vm.stack_depth() == 2
    assert vm.tos() == 0x0004
    assert vm.nos() == 0x0888


def test_UDHALF(vm):
    vm.push(0x8000)
    vm.push(0x0008)
    vm.execute("UDHALF")
    assert vm.stack_depth() == 2
    assert vm.tos() == 0x0004
    assert vm.nos() == 0x4000


def test_VAR(vm):
    cells = [vm.symbols["do_VAR"]]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 1


def test_CLS(vm):
    vm.push(1)
    vm.push(2)
    vm.push(3)
    vm.execute("CLS")
    assert vm.stack_depth() == 0


def test_DTOP(vm):
    vm.execute("DTOP")
    assert vm.stack_depth() == 1
    assert vm.peek(vm.tos()) == 0xF4


def test_BP(vm):
    vm.execute("_BP")
    assert vm.stack_depth() == 1
