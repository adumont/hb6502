NUM_BUF = 0x0210


def test_NUMBER_decimal(vm):
    for i, b in enumerate([0x23, 0x31, 0x32, 0x33]):
        vm.mpu.memory[NUM_BUF + i] = b
    vm.push(NUM_BUF)
    vm.push(4)
    vm.execute("NUMBER")
    assert vm.stack_depth() == 2
    flag, value = vm.tos(), vm.nos()
    assert flag != 0
    assert value == 123


def test_NUMBER_hex(vm):
    for i, b in enumerate([0x24, 0x46, 0x46]):
        vm.mpu.memory[NUM_BUF + i] = b
    vm.push(NUM_BUF)
    vm.push(3)
    vm.execute("NUMBER")
    assert vm.stack_depth() == 2
    flag, value = vm.tos(), vm.nos()
    assert flag != 0
    assert value == 0xFF


def test_NUMBER_binary(vm):
    for i, b in enumerate([0x25, 0x31, 0x30, 0x31, 0x30, 0x31]):
        vm.mpu.memory[NUM_BUF + i] = b
    vm.push(NUM_BUF)
    vm.push(6)
    vm.execute("NUMBER")
    assert vm.stack_depth() == 2
    flag, value = vm.tos(), vm.nos()
    assert flag != 0
    assert value == 0x15


def test_NUMBER_zero(vm):
    vm.mpu.memory[NUM_BUF] = 0x30
    vm.push(NUM_BUF)
    vm.push(1)
    vm.execute("NUMBER")
    assert vm.stack_depth() == 2
    flag, value = vm.tos(), vm.nos()
    assert flag != 0
    assert value == 0


def test_NUMBER_empty_string(vm):
    vm.push(NUM_BUF)
    vm.push(0)
    vm.execute("NUMBER")
    assert vm.stack_depth() == 2
    assert vm.tos() == 0


def test_NUMBER_invalid_prefix(vm):
    vm.mpu.memory[NUM_BUF] = 0x21
    vm.push(NUM_BUF)
    vm.push(1)
    vm.execute("NUMBER")
    assert vm.tos() == 0


def test_NUMBER_error_returns_zero_flag(vm):
    for i, b in enumerate([0x58, 0x59, 0x5A]):
        vm.mpu.memory[NUM_BUF + i] = b
    vm.push(NUM_BUF)
    vm.push(3)
    vm.execute("NUMBER")
    assert vm.tos() == 0


def test_NUMBER_octal(vm):
    for i, b in enumerate([0x6F, 0x37, 0x37, 0x37]):
        vm.mpu.memory[NUM_BUF + i] = b
    vm.push(NUM_BUF)
    vm.push(4)
    vm.execute("NUMBER")
    assert vm.stack_depth() == 2
    flag, value = vm.tos(), vm.nos()
    assert flag != 0
    assert value == 0o777


def test_NUMBER_FFFF(vm):
    for i, b in enumerate([0x24, 0x46, 0x46, 0x46, 0x46]):
        vm.mpu.memory[NUM_BUF + i] = b
    vm.push(NUM_BUF)
    vm.push(5)
    vm.execute("NUMBER")
    assert vm.stack_depth() == 2
    flag, value = vm.tos(), vm.nos()
    assert flag != 0
    assert value == 0xFFFF
