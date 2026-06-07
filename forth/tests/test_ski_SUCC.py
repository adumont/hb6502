def test_SKI_ZERO_SUCC_is_1(vm):
    vm.execute_thread(
        [
            vm.lookup("CN"),
            vm.lookup("ZERO"),
            vm.lookup("SUCC"),
            vm.lookup(")))"),
        ]
    )
    assert vm.stack_depth() == 1
    assert vm.tos() == 1

def test_SKI_ONE_SUCC_SUCC_is_3(vm):
    vm.execute_thread(
        [
            vm.lookup("CN"),
            vm.lookup("ONE"),
            vm.lookup("SUCC"),
            vm.lookup(")"),
            vm.lookup("SUCC"),
            vm.lookup(")"),
            vm.lookup("))"),
        ]
    )
    assert vm.stack_depth() == 1
    assert vm.tos() == 3

def test_SKI_TWO_SUCC_is_3(vm):
    vm.execute_thread(
        [
            vm.lookup("CN"),
            vm.lookup("TWO"),
            vm.lookup("SUCC"),
            vm.lookup(")))"),
        ]
    )
    assert vm.stack_depth() == 1
    assert vm.tos() == 3