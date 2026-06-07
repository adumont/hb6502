


def test_JUMP(vm):
    cells = [vm.symbols["do_JUMP"], vm.thread_addr + 4]
    vm.execute_thread(cells)


def test_0BR_branch_when_zero(vm):
    vm.push(0)
    cells = [vm.symbols["do_0BR"], vm.thread_addr + 4]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 0


def test_0BR_fallthrough_when_nonzero(vm):
    vm.push(42)
    cells = [vm.symbols["do_0BR"], 0x0000]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 0


def test_0BR_fallthrough_when_FFFF(vm):
    vm.push(0xFFFF)
    cells = [vm.symbols["do_0BR"], 0x0000]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 0


def test_0BR_pops_only_tos(vm):
    vm.push(99)
    vm.push(0)
    cells = [vm.symbols["do_0BR"], vm.thread_addr + 4]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 1
    assert vm.tos() == 99


def test_0BR_leaves_tos_when_nonzero(vm):
    vm.push(99)
    vm.push(42)
    cells = [vm.symbols["do_0BR"], 0x0000]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 1
    assert vm.tos() == 99


def test_I_after_star_do(vm):
    vm.push(10)
    vm.push(3)
    cells = [vm.symbols["do_STAR_DO"], vm.symbols["do_I"]]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 1
    assert vm.tos() == 3


def test_I_returns_index_from_rs(vm):
    vm.set_return_stack(100, 42)
    cells = [vm.symbols["do_I"]]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 1
    assert vm.tos() == 42


def test_J_returns_outer_index_from_rs(vm):
    vm.set_return_stack(200, 100, 50, 42)
    cells = [vm.symbols["do_J"]]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 1
    assert vm.tos() == 100


def test_star_loop_exits_when_I_ge_end(vm):
    vm.push(3)
    vm.push(3)
    cells = [
        vm.symbols["do_STAR_DO"],
        vm.symbols["do_I"],
        vm.symbols["do_STAR_LOOP"],
        0x0000,
    ]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 1
    assert vm.tos() == 3


def test_star_loop_continues_and_increments(vm):
    vm.push(5)
    vm.push(0)
    cells = [
        vm.symbols["do_STAR_DO"],
        vm.symbols["do_I"],
        vm.symbols["do_STAR_LOOP"],
        vm.thread_addr + 2,
    ]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 5
    assert vm.tos() == 4


def test_star_loop_runs_n_times(vm):
    vm.push(3)
    vm.push(0)
    cells = [
        vm.symbols["do_STAR_DO"],
        vm.symbols["do_I"],
        vm.symbols["do_STAR_LOOP"],
        vm.thread_addr + 2,
    ]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 3
    assert vm.tos() == 2


def test_star_do_leaves_data_stack_empty(vm):
    vm.push(20)
    vm.push(10)
    cells = [vm.symbols["do_STAR_DO"]]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 0
