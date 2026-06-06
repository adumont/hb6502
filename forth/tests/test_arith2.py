def test_ULESS_unsigned_true(vm):
    vm.push(0x0001)
    vm.push(0x0002)
    vm.execute("ULESS")
    assert vm.tos() == 0xFFFF


def test_ULESS_unsigned_false(vm):
    vm.push(0x0002)
    vm.push(0x0001)
    vm.execute("ULESS")
    assert vm.tos() == 0x0000


def test_ULESS_unsigned_equal(vm):
    vm.push(0x0002)
    vm.push(0x0002)
    vm.execute("ULESS")
    assert vm.tos() == 0x0000


def test_ULESS_wraparound(vm):
    vm.push(0x0001)
    vm.push(0x8000)
    vm.execute("ULESS")
    assert vm.tos() == 0xFFFF


def test_ULESS_zero_equal(vm):
    vm.push(0x0000)
    vm.push(0x0000)
    vm.execute("ULESS")
    assert vm.tos() == 0x0000


def test_ULESS_FFFF_vs_zero(vm):
    vm.push(0xFFFF)
    vm.push(0x0000)
    vm.execute("ULESS")
    assert vm.tos() == 0x0000


def test_ULESS_zero_vs_FFFF(vm):
    vm.push(0x0000)
    vm.push(0xFFFF)
    vm.execute("ULESS")
    assert vm.tos() == 0xFFFF


def test_UM_STAR(vm):
    vm.push(0x0003)
    vm.push(0x0005)
    vm.execute("UM_STAR")
    assert vm.stack_depth() == 2
    hi, lo = vm.tos(), vm.nos()
    assert (hi << 16) | lo == 15


def test_UM_STAR_large(vm):
    vm.push(0x0100)
    vm.push(0x0100)
    vm.execute("UM_STAR")
    assert vm.stack_depth() == 2
    hi, lo = vm.tos(), vm.nos()
    assert (hi << 16) | lo == 0x10000


def test_UM_STAR_max_times_max(vm):
    vm.push(0xFFFF)
    vm.push(0xFFFF)
    vm.execute("UM_STAR")
    assert vm.stack_depth() == 2
    hi, lo = vm.tos(), vm.nos()
    assert (hi << 16) | lo == 0xFFFE0001


def test_UM_STAR_zero(vm):
    vm.push(0xFFFF)
    vm.push(0x0000)
    vm.execute("UM_STAR")
    assert vm.tos() == 0
    assert vm.nos() == 0


def test_DIV10(vm):
    vm.push(100)
    vm.execute("DIV10")
    assert vm.tos() == 10


def test_DIV10_remainder(vm):
    vm.push(107)
    vm.execute("DIV10")
    assert vm.tos() == 10


def test_DIV10_zero(vm):
    vm.push(0)
    vm.execute("DIV10")
    assert vm.tos() == 0


def test_DIV10_max(vm):
    vm.push(0xFFFF)
    vm.execute("DIV10")
    assert vm.tos() == 0x1999


def test_testNEG_positive(vm):
    vm.push(0x0001)
    vm.execute("testNEG")
    assert vm.tos() == 0x0000


def test_testNEG_negative(vm):
    vm.push(0x8000)
    vm.execute("testNEG")
    assert vm.tos() == 0xFFFF


def test_testNEG_zero(vm):
    vm.push(0x0000)
    vm.execute("testNEG")
    assert vm.tos() == 0x0000


def test_testNEG_FFFF(vm):
    vm.push(0xFFFF)
    vm.execute("testNEG")
    assert vm.tos() == 0xFFFF


def test_testNEG_7FFF(vm):
    vm.push(0x7FFF)
    vm.execute("testNEG")
    assert vm.tos() == 0x0000


def test_DPLUS(vm):
    vm.push(0x0001)
    vm.push(0x0002)
    vm.push(0x0003)
    vm.push(0x0004)
    vm.execute("DPLUS")
    assert vm.stack_depth() == 2
    hi, lo = vm.tos(), vm.nos()
    assert (hi << 16) | lo == 0x00060004


def test_DPLUS_overflow(vm):
    vm.push(0xFFFF)
    vm.push(0xFFFF)
    vm.push(0x0001)
    vm.push(0x0000)
    vm.execute("DPLUS")
    assert vm.stack_depth() == 2
    hi, lo = vm.tos(), vm.nos()
    assert (hi << 16) | lo == 0x00000000


def test_DMINUS_underflow(vm):
    vm.push(0x0000)
    vm.push(0x0000)
    vm.push(0x0001)
    vm.push(0x0000)
    vm.execute("DMINUS")
    assert vm.stack_depth() == 2
    hi, lo = vm.tos(), vm.nos()
    assert (hi << 16) | lo == 0xFFFFFFFF


def test_DMINUS(vm):
    vm.push(0x000A)
    vm.push(0x0006)
    vm.push(0x0003)
    vm.push(0x0004)
    vm.execute("DMINUS")
    assert vm.stack_depth() == 2
    hi, lo = vm.tos(), vm.nos()
    assert (hi << 16) | lo == 0x00020007


def test_STAR_UM_DIV_MOD_simple(vm):
    vm.push(0x000F)
    vm.push(0x0000)
    vm.push(4)
    cells = [vm.symbols["do_STAR_UM_DIV_MOD"]]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 2
    rem, quot = vm.tos(), vm.nos()
    assert quot == 3
    assert rem == 3


def test_STAR_UM_DIV_MOD_exact(vm):
    vm.push(0x0000)
    vm.push(0x0001)
    vm.push(0x0100)
    cells = [vm.symbols["do_STAR_UM_DIV_MOD"]]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 2
    rem, quot = vm.tos(), vm.nos()
    assert quot == 0x0100
    assert rem == 0x0000


def test_STAR_UM_DIV_MOD_large_dividend(vm):
    vm.push(0xFF00)
    vm.push(0x0010)
    vm.push(0x0100)
    cells = [vm.symbols["do_STAR_UM_DIV_MOD"]]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 2
    rem, quot = vm.tos(), vm.nos()
    assert quot == 0x10FF
    assert rem == 0


def test_STAR_UM_DIV_MOD_overflow_returns_ffff(vm):
    vm.push(0x0000)
    vm.push(0x8000)
    vm.push(0x0001)
    cells = [vm.symbols["do_STAR_UM_DIV_MOD"]]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 2
    assert vm.tos() == 0xFFFF
    assert vm.nos() == 0xFFFF


def test_STAR_UM_DIV_MOD_zero_divisor(vm):
    vm.push(0x1234)
    vm.push(0x5678)
    vm.push(0x0000)
    cells = [vm.symbols["do_STAR_UM_DIV_MOD"]]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 2
    assert vm.tos() == 0xFFFF
    assert vm.nos() == 0xFFFF
