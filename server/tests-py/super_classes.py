import pytest
from abc import ABC, abstractmethod
import os

class DefaultTestQueries(ABC):

    def do_setup(self, setup_ctrl, hge_ctx):
        if not setup_ctrl['setupDone']:
            hge_ctx.v1q_f(self.dir() + '/setup.yaml')
            setup_ctrl['setupDone'] = True

    def do_teardown(self, setup_ctrl, hge_ctx):
        if setup_ctrl['setupDone'] and not hge_ctx.may_skip_test_teardown:
            hge_ctx.v1q_f(self.dir() + '/teardown.yaml')
            setup_ctrl['setupDone'] = False

    @pytest.fixture(autouse=True)
    def transact(self, setup_ctrl, hge_ctx):
        self.do_setup(setup_ctrl, hge_ctx)
        yield
        self.do_teardown(setup_ctrl, hge_ctx);

    @abstractmethod
    def dir(self) -> str:
        pass

class DefaultTestMutations(ABC):

    @pytest.fixture(scope='class')
    def schema_transact(self, request, hge_ctx):
        hge_ctx.v1q_f(self.dir() + '/schema_setup.yaml')
        yield
        hge_ctx.v1q_f(self.dir() + '/schema_teardown.yaml')

    @pytest.fixture(autouse=True)
    def init_values_transact(self, schema_transact, hge_ctx):
        setupValFile = self.dir() + '/values_setup.yaml'
        if os.path.isfile(setupValFile):
          hge_ctx.v1q_f(setupValFile)
        yield
        hge_ctx.v1q_f(self.dir() + '/values_teardown.yaml')

    @abstractmethod
    def dir(self) -> str:
        pass


# Any test which has a setup and a teardown
# Ideally, DefaultTestSelectQueries should just be this
class GraphQLEngineTest(ABC):

    @pytest.fixture(scope='class')
    def transact(self, request, hge_ctx):
        hge_ctx.v1q_f(self.dir() + '/setup.yaml')
        yield
        hge_ctx.v1q_f(self.dir() + '/teardown.yaml')

    @pytest.fixture(autouse=True)
    def ensure_transact(self, transact):
        pass

    @abstractmethod
    def dir(self) -> str:
        pass

class DefaultTestSelectQueries(GraphQLEngineTest):
    pass
