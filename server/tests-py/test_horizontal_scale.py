import pytest
import ruamel.yaml as yaml
import time
import jsondiff
from context import PytestConf


if not PytestConf.config.getoption("--test-hge-scale-url"):
    pytest.skip("--test-hge-scale-url flag is missing, skipping tests", allow_module_level=True)


class TestHorizantalScaleBasic():
    servers = {}

    @pytest.fixture(autouse=True, scope='class')
    def transact(self, hge_ctx):
        self.servers['1'] = hge_ctx.hge_url
        self.servers['2'] = hge_ctx.hge_scale_url
        yield
        # teardown
        st_code, resp = hge_ctx.v1q_f(self.dir() + '/teardown.yaml')
        assert st_code == 200, resp

    def test_horizontal_scale_basic(self, hge_ctx):
        with open(self.dir() + "/steps.yaml") as c:
            conf = yaml.safe_load(c)

        assert isinstance(conf, list) == True, 'Not a list'
        for _, step in enumerate(conf):
            # execute operation
            response = hge_ctx.http.post(
                self.servers[step['operation']['server']] + "/v1/query",
                json=step['operation']['query']
            )
            st_code = response.status_code
            resp = response.json()
            assert st_code == 200, resp

            # wait for 20 sec
            time.sleep(20)
            # validate data
            response = hge_ctx.http.post(
                self.servers[step['validate']['server']] + "/v1alpha1/graphql",
                json=step['validate']['query']
            )
            st_code = response.status_code
            resp = response.json()
            assert st_code == 200, resp

            if 'response' in step['validate']:
                assert resp == step['validate']['response'], yaml.dump({
                    'response': resp,
                    'expected': step['validate']['response'],
                    'diff': jsondiff.diff(step['validate']['response'], resp)
                })

    @classmethod
    def dir(cls):
        return 'queries/horizontal_scale/basic'
