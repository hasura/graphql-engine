import pytest
import yaml
import time
import jsondiff

from validate import json_ordered


if not pytest.config.getoption("--test-hge-scale-url"):
    pytest.skip("--test-hge-scale-url flag is missing, skipping tests", allow_module_level=True)


class TestHorizantalScaleBasic():

    @pytest.fixture(autouse=True, scope='class')
    def transact(self, hge_ctx):
        self.teardown = {"type": "clear_metadata", "args": {}}
        yield
        # teardown
        st_code, resp = hge_ctx.v1q(self.teardown)
        assert st_code == 200, resp
    
    def test_horizontal_scale_basic(self, hge_ctx):
        with open(self.dir() + "/steps.yaml") as c:
            conf = yaml.load(c)
        
        assert isinstance(conf, list) == True, 'Not an list'
        for _, step in enumerate(conf):
            # execute operation on 1st server
            st_code, resp = hge_ctx.v1q(step['operation'])
            assert st_code == 200, resp

            # wait for x sec
            time.sleep(0.3)
            # validate data on 2nd server
            response = hge_ctx.http.post(
                hge_ctx.hge_scale_url + "/v1alpha1/graphql",
                json=step['validate']['query']
            )
            st_code = response.status_code
            resp = response.json()
            assert st_code == 200, resp

            if 'response' in step['validate']:
                assert json_ordered(resp) == json_ordered(step['validate']['response']), yaml.dump({
                    'response': resp,
                    'expected': step['validate']['response'],
                    'diff': jsondiff.diff(step['validate']['response'], resp)
                })
    
    @classmethod
    def dir(cls):
        return 'queries/horizontal_scale/basic'
