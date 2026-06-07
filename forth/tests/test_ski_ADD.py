def test_SKI_ONE_TOO_ADD_is_3(vm):
    vm.execute_thread(
        [
            vm.lookup("CN"),
            vm.lookup("ONE"),
            vm.lookup("TWO"),
            vm.lookup("ADD"),
            vm.lookup("))"),
            vm.lookup("))"),
        ]
    )
    assert vm.stack_depth() == 1
    assert vm.tos() == 3