def test_NOT_TRUE_is_FALSE(vm):
    vm.push(vm.symbols["do_FALSE"])
    vm.push(vm.symbols["do_TRUE"])
    vm.execute_thread(
        [
            vm.lookup("T"),
            vm.lookup("NOT"),
            vm.lookup(")"),
            vm.lookup(")))"),
        ]
    )
    assert vm.stack_depth() == 1
    assert vm.tos() == 0


def test_NOT_FALSE_is_TRUE(vm):
    vm.push(vm.symbols["do_FALSE"])
    vm.push(vm.symbols["do_TRUE"])
    vm.execute_thread(
        [
            vm.lookup("F"),
            vm.lookup("NOT"),
            vm.lookup(")"),
            vm.lookup(")))"),
        ]
    )
    assert vm.stack_depth() == 1
    assert vm.tos() == 0xFFFF
