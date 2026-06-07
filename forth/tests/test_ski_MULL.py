def test_SKI_CN_ONE_TWO_MUL_is_2(vm):
    vm.execute_thread(
        [
            vm.lookup("CN"),
            vm.lookup("ONE"),
            vm.lookup("TWO"),
            vm.lookup("MUL"),
            vm.lookup("))"),
            vm.lookup("))"),
        ]
    )
    assert vm.stack_depth() == 1
    assert vm.tos() == 2


def test_SKI_CN_TWO_THREE_MUL_is_6(vm):
    vm.execute_thread(
        [
            vm.lookup("CN"),
            vm.lookup("TWO"),
            vm.lookup("THREE"),
            vm.lookup("MUL"),
            vm.lookup("))"),
            vm.lookup("))"),
        ]
    )
    assert vm.stack_depth() == 1
    assert vm.tos() == 6


def test_SKI_CN_THREE_TWO_MUL_is_6(vm):
    vm.execute_thread(
        [
            vm.lookup("CN"),
            vm.lookup("THREE"),
            vm.lookup("TWO"),
            vm.lookup("MUL"),
            vm.lookup("))"),
            vm.lookup("))"),
        ]
    )
    assert vm.stack_depth() == 1
    assert vm.tos() == 6


def test_SKI_CN_TWO_ZERO_MUL_is_0(vm):
    vm.execute_thread(
        [
            vm.lookup("CN"),
            vm.lookup("TWO"),
            vm.lookup("ZERO"),
            vm.lookup("MUL"),
            vm.lookup("))"),
            vm.lookup("))"),
        ]
    )
    assert vm.stack_depth() == 1
    assert vm.tos() == 0