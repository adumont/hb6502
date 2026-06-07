def test_SKI_TWO_THREE_is_8(vm):
    vm.execute_thread(
        [
            vm.lookup("CN"),
            vm.lookup("TWO"),
            vm.lookup("THREE"),
            vm.lookup(")))"),
        ]
    )
    assert vm.stack_depth() == 1
    assert vm.tos() == 8

def test_SKI_THREE_TWO_is_9(vm):
    vm.execute_thread(
        [
            vm.lookup("CN"),
            vm.lookup("THREE"),
            vm.lookup("TWO"),
            vm.lookup(")))"),
        ]
    )
    assert vm.stack_depth() == 1
    assert vm.tos() == 9