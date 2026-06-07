def test_AND_TRUE_TRUE_is_TRUE(vm):
    vm.push(vm.symbols["do_FALSE"])
    vm.push(vm.symbols["do_TRUE"])
    vm.execute_thread(
        [
            vm.lookup("T"),
            vm.lookup("T"),
            vm.lookup("AND"),
            vm.lookup("))"),
            vm.lookup(")))"),
        ]
    )
    assert vm.stack_depth() == 1
    assert vm.tos() == 0xFFFF


def test_AND_FALSE_TRUE_is_TRUE(vm):
    vm.push(vm.symbols["do_FALSE"])
    vm.push(vm.symbols["do_TRUE"])
    vm.execute_thread(
        [
            vm.lookup("F"),
            vm.lookup("T"),
            vm.lookup("AND"),
            vm.lookup("))"),
            vm.lookup(")))"),
        ]
    )
    assert vm.stack_depth() == 1
    assert vm.tos() == 0


def test_AND_TRUE_FALSE_is_FALSE(vm):
    vm.push(vm.symbols["do_FALSE"])
    vm.push(vm.symbols["do_TRUE"])
    vm.execute_thread(
        [
            vm.lookup("T"),
            vm.lookup("F"),
            vm.lookup("AND"),
            vm.lookup("))"),
            vm.lookup(")))"),
        ]
    )
    assert vm.stack_depth() == 1
    assert vm.tos() == 0


def test_AND_FALSE_FALSE_is_FALSE(vm):
    vm.push(vm.symbols["do_FALSE"])
    vm.push(vm.symbols["do_TRUE"])
    vm.execute_thread(
        [
            vm.lookup("F"),
            vm.lookup("F"),
            vm.lookup("AND"),
            vm.lookup("))"),
            vm.lookup(")))"),
        ]
    )
    assert vm.stack_depth() == 1
    assert vm.tos() == 0
