import pytest


def test_STATE(vm):
    vm.execute("STATE")
    assert vm.stack_depth() == 1


def test_MODE(vm):
    vm.execute("MODE")
    assert vm.stack_depth() == 1


@pytest.mark.skip(
    reason="COLON/SEMI are colon-mechanism primitives; tested implicitly by execute()"
)
def test_COLON_SEMI(vm):
    pass


def test_COMPILE(vm):
    vm.execute("HERE")
    old_here = vm.tos()
    vm.reset_stack()
    cells = [
        vm.symbols["do_COMPILE"],
        vm.symbols["do_PUSH0"],
    ]
    vm.execute_thread(cells)
    compiled = vm._get_word(old_here)
    assert compiled == vm.symbols["do_PUSH0"]
    vm.reset_stack()
    vm.execute("HERE")
    assert vm.tos() == old_here + 2


def test_LBRAC(vm):
    vm.execute("LBRAC")
    vm.reset_stack()
    vm.execute("STATE")
    assert vm.tos() == 1


def test_RBRAC(vm):
    vm.execute("RBRAC")
    vm.reset_stack()
    vm.execute("STATE")
    assert vm.tos() == 0


@pytest.mark.skip(reason="SETIMM requires dictionary header")
def test_SETIMM(vm):
    pass


@pytest.mark.skip(reason="GETIMM requires dictionary header")
def test_GETIMM(vm):
    pass


@pytest.mark.skip(reason="FCOLON requires input stream")
def test_FCOLON(vm):
    pass
