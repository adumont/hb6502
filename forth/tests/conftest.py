import pytest

from .helpers import ForthTestVM


@pytest.fixture
def vm():
    _vm = ForthTestVM()
    _vm.reset_stack()
    yield _vm
