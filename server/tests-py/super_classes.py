import pytest
from abc import ABC, abstractmethod
import os

class DefaultTestQueries(ABC):

    def do_setup(self, setup_ctrl, hge_ctx):
        if not setup_ctrl['setupDone']:
            st_code, resp = hge_ctx.v1q_f(self.dir() + '/setup.yaml')
            assert st_code == 200, resp
            setup_ctrl['setupDone'] = True

    def do_teardown(self, setup_ctrl, hge_ctx):
        if setup_ctrl['setupDone'] and not hge_ctx.may_skip_test_teardown:
            st_code, resp = hge_ctx.v1q_f(self.dir() + '/teardown.yaml')
            assert st_code == 200, resp
            setup_ctrl['setupDone'] = False

    @pytest.fixture(autouse=True)
    def transact(self, setup_ctrl, hge_ctx):
        self.do_setup(setup_ctrl, hge_ctx)
        yield
        self.do_teardown(setup_ctrl, hge_ctx);

    @abstractmethod
    def dir(self):
        pass

class DefaultTestMutations(ABC):

    @pytest.fixture(scope='class')
    def schema_transact(self, request, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + '/schema_setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir() + '/schema_teardown.yaml')
        assert st_code == 200, resp

    @pytest.fixture(autouse=True)
    def init_values_transact(self, schema_transact, hge_ctx):
        setupValFile = self.dir() + '/values_setup.yaml'
        if os.path.isfile(setupValFile):
          st_code, resp = hge_ctx.v1q_f(setupValFile)
          assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir() + '/values_teardown.yaml')
        assert st_code == 200, resp

    @abstractmethod
    def dir(self):
        pass



class DefaultTestSelectQueries(ABC):

    @pytest.fixture(scope='class')
    def transact(self, request, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir() + '/teardown.yaml')
        assert st_code == 200, resp

    @pytest.fixture(autouse=True)
    def ensure_transact(self, transact):
        pass

    @abstractmethod
    def dir(self):
        pass
