import contextlib

import pytest

from .helpers import DTOP_VALUE


def test_ABORT_resets_data_stack(vm):
    vm.push(1)
    vm.push(2)
    with contextlib.suppress(RuntimeError):
        vm.execute("ABORT")
    assert vm.mpu.x == DTOP_VALUE


def test_EXEC(vm):
    vm.push(vm.symbols["do_PUSH0"])
    vm.execute("EXEC")
    assert vm.tos() == 0


@pytest.mark.skip(reason="CREATE requires input stream")
def test_CREATE(vm):
    pass


@pytest.mark.skip(reason="VARIABLE requires input stream")
def test_VARIABLE(vm):
    pass


@pytest.mark.skip(reason="CODE requires input stream")
def test_CODE(vm):
    pass


def test_END_CODE(vm):
    cells = [vm.symbols["do_END_CODE"]]
    vm.execute_thread(cells)


@pytest.mark.skip(reason="CREATED requires RS setup and colon context")
def test_CREATED(vm):
    pass


@pytest.mark.skip(reason="NONAME requires colon context")
def test_NONAME(vm):
    pass


@pytest.mark.skip(reason="MARKER requires input stream")
def test_MARKER(vm):
    pass
