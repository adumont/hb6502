import pytest
from .helpers import THREAD_ADDR


def test_HIDE(vm):
    cells = [vm.symbols['do_HIDE']]
    vm.execute_thread(cells)


def test_UNHIDE(vm):
    cells = [vm.symbols['do_UNHIDE']]
    vm.execute_thread(cells)


def test_REVEAL(vm):
    cells = [vm.symbols['do_REVEAL']]
    vm.execute_thread(cells)


def test_HIDDEN(vm):
    cells = [vm.symbols['do_HIDDEN']]
    vm.execute_thread(cells)


def test_CLEAR_OK(vm):
    cells = [vm.symbols['do_CLEAR_OK']]
    vm.execute_thread(cells)


def test_FETCH_OK(vm):
    cells = [vm.symbols['do_FETCH_OK']]
    vm.execute_thread(cells)
    assert vm.stack_depth() == 1


STR_BUF = 0x0220


def test_FIND_finds_DUP(vm):
    for i, b in enumerate([0x44, 0x55, 0x50]):
        vm.mpu.memory[STR_BUF + i] = b
    vm.push(STR_BUF)
    vm.push(3)
    vm.execute('FIND')
    assert vm.stack_depth() == 1
    assert vm.tos() != 0


def test_FIND_finds_SWAP(vm):
    for i, b in enumerate([0x53, 0x57, 0x41, 0x50]):
        vm.mpu.memory[STR_BUF + i] = b
    vm.push(STR_BUF)
    vm.push(4)
    vm.execute('FIND')
    assert vm.stack_depth() == 1
    assert vm.tos() != 0


def test_FIND_not_found(vm):
    for i, b in enumerate([0x58, 0x59, 0x5A, 0x5A, 0x5A]):
        vm.mpu.memory[STR_BUF + i] = b
    vm.push(STR_BUF)
    vm.push(5)
    vm.execute('FIND')
    assert vm.stack_depth() == 1
    assert vm.tos() == 0


def test_FIND_finds_DROP(vm):
    for i, b in enumerate([0x44, 0x52, 0x4F, 0x50]):
        vm.mpu.memory[STR_BUF + i] = b
    vm.push(STR_BUF)
    vm.push(4)
    vm.execute('FIND')
    assert vm.stack_depth() == 1
    assert vm.tos() != 0
