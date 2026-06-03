from .helpers import THREAD_ADDR


def test_LIT(vm):
    cells = [vm.symbols['do_LIT'], 0x1234]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 1
    assert vm.tos() == 0x1234


def test_LIT_negative(vm):
    cells = [vm.symbols['do_LIT'], 0x8000]
    vm.execute_thread(cells)
    assert vm.tos() == 0x8000


def test_LEAVE_sets_I_to_end(vm):
    vm.set_return_stack(THREAD_ADDR + 6)
    vm.push(10)
    vm.push(0)
    cells = [
        vm.symbols['do_STAR_DO'],
        vm.symbols['do_LEAVE'],
        vm.symbols['do_I'],
    ]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 1
    assert vm.tos() == 10


def test_QDO_skips_when_equal(vm):
    vm.push(5)
    vm.push(5)
    cells = [vm.symbols['do_STAR_SKIP_DO'], THREAD_ADDR + 6, vm.symbols['do_I']]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 0


def test_PLUS_LOOP_positive_inc(vm):
    vm.push(5)
    vm.push(0)
    cells = [
        vm.symbols['do_STAR_DO'],
        vm.symbols['do_I'],
        vm.symbols['do_LIT'], 2,
        vm.symbols['do_STAR_PLUS_LOOP'],
        THREAD_ADDR + 2,
    ]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 3
    assert vm.tos() == 4


def test_PLUS_LOOP_negative_inc_exits_on_first_check(vm):
    vm.push(0)
    vm.push(5)
    cells = [
        vm.symbols['do_STAR_DO'],
        vm.symbols['do_I'],
        vm.symbols['do_LIT'], 0xFFFF,
        vm.symbols['do_STAR_PLUS_LOOP'],
        THREAD_ADDR + 2,
    ]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 1
    assert vm.tos() == 5


TRAP_LO = 0x00
TRAP_HI = 0x03


def test_CLIT_literal_byte(vm):
    clit = vm.symbols['do_CLIT']
    raw = [clit & 0xFF, (clit >> 8) & 0xFF, 0x42, TRAP_LO, TRAP_HI]
    vm.execute_raw(raw)
    assert vm.stack_depth() == 1
    assert vm.tos() == 0x0042


def test_CLIT_zero(vm):
    clit = vm.symbols['do_CLIT']
    raw = [clit & 0xFF, (clit >> 8) & 0xFF, 0x00, TRAP_LO, TRAP_HI]
    vm.execute_raw(raw)
    assert vm.stack_depth() == 1
    assert vm.tos() == 0x0000


def test_CLIT_high_byte(vm):
    clit = vm.symbols['do_CLIT']
    raw = [clit & 0xFF, (clit >> 8) & 0xFF, 0xFF, TRAP_LO, TRAP_HI]
    vm.execute_raw(raw)
    assert vm.stack_depth() == 1
    assert vm.tos() == 0x00FF


def test_LITSTR_pushes_addr(vm):
    litstr = vm.symbols['do_LITSTR']
    raw = [litstr & 0xFF, (litstr >> 8) & 0xFF, 3, 0x41, 0x42, 0x43, TRAP_LO, TRAP_HI]
    vm.execute_raw(raw)
    assert vm.stack_depth() == 1
    addr = vm.tos()
    assert vm.peek(addr) == 3
    assert vm.peek(addr + 1) == 0x41
    assert vm.peek(addr + 2) == 0x42
    assert vm.peek(addr + 3) == 0x43
