def test_K(vm):
    vm.push(vm.symbols["do_FALSE"])
    vm.push(vm.symbols["do_TRUE"])
    vm.execute_thread(
        [
            vm.lookup("K"),
            vm.lookup(")))"),
        ]
    )
    assert vm.stack_depth() == 1
    assert vm.tos() == 0xFFFF  # TRUE


def test_KI(vm):
    vm.push(vm.symbols["do_PUSH1"])
    vm.push(vm.symbols["do_TRUE"])
    vm.execute_thread(
        [
            vm.lookup("KI"),
            vm.lookup(")))"),
        ]
    )
    assert vm.stack_depth() == 1
    assert vm.tos() == 1
