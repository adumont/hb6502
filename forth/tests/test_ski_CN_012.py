def test_SKI_CN_ZERO_is_0(vm):
    vm.execute_thread(
        [
            vm.lookup("CN"),
            vm.lookup("ZERO"),
            vm.lookup("))"),
        ]
    )
    assert vm.stack_depth() == 1
    assert vm.tos() == 0

def test_SKI_CN_ONE_is_1(vm):
    vm.execute_thread(
        [
            vm.lookup("CN"),
            vm.lookup("ONE"),
            vm.lookup("))"),
        ]
    )
    assert vm.stack_depth() == 1
    assert vm.tos() == 1

def test_SKI_CN_TWO_is_2(vm):
    vm.execute_thread(
        [
            vm.lookup("CN"),
            vm.lookup("TWO"),
            vm.lookup("))"),
        ]
    )
    assert vm.stack_depth() == 1
    assert vm.tos() == 2