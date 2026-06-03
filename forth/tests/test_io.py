def test_EMIT(vm):
    vm.push(0x0041)
    vm.execute('EMIT')
    assert vm.stack_depth() == 0


def test_CR(vm):
    vm.execute('CR')
    assert vm.stack_depth() == 0


def test_SPACE(vm):
    vm.execute('SPACE')
    assert vm.stack_depth() == 0


def test_COUNT(vm):
    vm.poke(0x0200, 3)
    vm.poke(0x0201, 0x41)
    vm.poke(0x0202, 0x42)
    vm.poke(0x0203, 0x43)
    vm.push(0x0200)
    vm.execute('COUNT')
    assert vm.stack_depth() == 2
    length, addr = vm.tos(), vm.nos()
    assert length == 3
    assert addr == 0x0201


def test_CLS(vm):
    vm.push(1)
    vm.push(2)
    vm.execute('CLS')
    assert vm.stack_depth() == 0


def test_BREAK(vm):
    vm.push(0x1234)
    cells = [vm.symbols['do_BREAK'], vm.symbols['do_DROP']]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 0


def test_PRINT_drops_value(vm):
    vm.push(0x1234)
    vm.execute('PRINT')
    assert vm.stack_depth() == 0


def test_CPRINT_drops_value(vm):
    vm.push(0x0042)
    vm.execute('CPRINT')
    assert vm.stack_depth() == 0


def test_DPRINT_drops_two_cells(vm):
    vm.push(0x1111)
    vm.push(0x2222)
    vm.execute('DPRINT')
    assert vm.stack_depth() == 0


def test_TYPE_prints_and_drops(vm):
    vm.poke(0x0200, 3)
    vm.poke(0x0201, 0x41)
    vm.poke(0x0202, 0x42)
    vm.poke(0x0203, 0x43)
    vm.push(0x0200)
    vm.push(3)
    vm.execute('TYPE')
    assert vm.stack_depth() == 0


def test_TYPE_empty_string(vm):
    vm.push(0x0200)
    vm.push(0)
    vm.execute('TYPE')
    assert vm.stack_depth() == 0


def test_WORD_parses_from_input(vm):
    vm.mpu.memory[0x0208] = 4
    vm.mpu.memory[0x0209] = 0x41
    vm.mpu.memory[0x020A] = 0x42
    vm.mpu.memory[0x020B] = 0x43
    vm.mpu.memory[0x020C] = 0x0A
    vm.mpu.memory[0x0289] = 0
    vm.execute('WORD')
    assert vm.stack_depth() == 2
    length, addr = vm.tos(), vm.nos()
    assert length == 3
    assert vm.peek(addr) == 0x41


def test_WORD_at_end_of_input(vm):
    vm.mpu.memory[0x0208] = 1
    vm.mpu.memory[0x0209] = 0x0A
    vm.mpu.memory[0x0289] = 0
    vm.execute('WORD')
    assert vm.stack_depth() == 2
    assert vm.tos() == 0
    assert vm.nos() == 0


def test_PARSE_with_custom_sep(vm):
    vm.mpu.memory[0x0208] = 6
    vm.mpu.memory[0x0209] = 0x2F
    vm.mpu.memory[0x020A] = 0x61
    vm.mpu.memory[0x020B] = 0x2F
    vm.mpu.memory[0x020C] = 0x62
    vm.mpu.memory[0x020D] = 0x63
    vm.mpu.memory[0x020E] = 0x0A
    vm.mpu.memory[0x0289] = 0
    vm.push(0x002F)
    vm.execute('PARSE')
    assert vm.stack_depth() == 2
    length, addr = vm.tos(), vm.nos()
    assert length == 1
    assert vm.peek(addr) == 0x61
