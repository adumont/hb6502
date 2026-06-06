def test_TO_R(vm):
    vm.push(0x1234)
    cells = [vm.symbols["do_TO_R"]]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 0
    assert vm.mpu.memory[0x100 + vm.mpu.sp + 1] == 0x34
    assert vm.mpu.memory[0x100 + vm.mpu.sp + 2] == 0x12


def test_FROM_R(vm):
    vm.set_return_stack(0xABCD)
    cells = [vm.symbols["do_FROM_R"]]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 1
    assert vm.tos() == 0xABCD


def test_FROM_R_pops(vm):
    vm.set_return_stack(0x1111, 0x2222)
    cells = [vm.symbols["do_FROM_R"], vm.symbols["do_FROM_R"]]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 2
    assert vm.tos() == 0x1111
    assert vm.nos() == 0x2222


def test_R_AT(vm):
    vm.set_return_stack(0xABCD, 0x1234)
    cells = [vm.symbols["do_R_AT"]]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 1
    assert vm.tos() == 0x1234


def test_R_AT_does_not_pop(vm):
    vm.set_return_stack(0xAAAA, 0xBBBB)
    cells = [vm.symbols["do_R_AT"], vm.symbols["do_R_AT"]]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 2
    assert vm.tos() == 0xBBBB
    assert vm.nos() == 0xBBBB


def test_TO_R_then_FROM_R(vm):
    vm.push(0xDEAD)
    cells = [vm.symbols["do_TO_R"], vm.symbols["do_FROM_R"]]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 1
    assert vm.tos() == 0xDEAD


def test_TO_R_stack_deep(vm):
    vm.push(1)
    vm.push(2)
    vm.push(3)
    cells = [vm.symbols["do_TO_R"], vm.symbols["do_TO_R"], vm.symbols["do_TO_R"]]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 0


def test_RP_FETCH_initial(vm):
    sp_before = vm.mpu.sp
    vm.execute("RP_FETCH")
    assert vm.stack_depth() == 1
    assert vm.tos() == sp_before


def test_RP_STORE_restores(vm):
    original_sp = vm.mpu.sp
    vm.push(0x00FF)
    vm.execute("RP_STORE")
    assert vm.mpu.sp == 0xFF
    vm.mpu.sp = original_sp
