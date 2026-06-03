def test_KEY_returns_char_from_input(vm):
    vm.mpu.memory[0x0208] = 5
    vm.mpu.memory[0x0209] = 0x48
    vm.mpu.memory[0x020A] = 0x65
    vm.mpu.memory[0x020B] = 0x6C
    vm.mpu.memory[0x020C] = 0x6C
    vm.mpu.memory[0x020D] = 0x6F
    vm.mpu.memory[0x0289] = 0
    vm.execute('KEY')
    assert vm.stack_depth() == 1
    assert vm.tos() == 0x0048


def test_KEY_advances_index(vm):
    vm.mpu.memory[0x0208] = 3
    vm.mpu.memory[0x0209] = 0x41
    vm.mpu.memory[0x020A] = 0x42
    vm.mpu.memory[0x020B] = 0x43
    vm.mpu.memory[0x0289] = 0
    vm.execute('KEY')
    assert vm.tos() == 0x0041
    vm.reset_stack()
    vm.execute('KEY')
    assert vm.tos() == 0x0042
    vm.reset_stack()
    vm.execute('KEY')
    assert vm.tos() == 0x0043


def test_GETC_reads_from_io_port(vm):
    vm.mpu.memory[0xF004] = 0x5A
    vm.execute('GETC')
    assert vm.stack_depth() == 1
    assert vm.tos() == 0x005A


def test_GETC_multiple_calls(vm):
    vm.mpu.memory[0xF004] = 0x31
    vm.execute('GETC')
    assert vm.tos() == 0x0031
    vm.reset_stack()
    vm.mpu.memory[0xF004] = 0x32
    vm.execute('GETC')
    assert vm.tos() == 0x0032
    vm.reset_stack()
    vm.mpu.memory[0xF004] = 0x33
    vm.execute('GETC')
    assert vm.tos() == 0x0033
