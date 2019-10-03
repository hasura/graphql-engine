import pytest
import yaml
import json
import jsondiff

from validate import json_ordered

class TestInconsistentObjects():

    get_inconsistent_metadata = {
        "type": "get_inconsistent_metadata",
        "args": {}
    }
    reload_metadata = {
        "type": "reload_metadata",
        "args": {}
    }
    drop_inconsistent_metadata = {
        "type": "drop_inconsistent_metadata",
        "args": {}
    }
    export_metadata = {
        "type": "export_metadata",
        "args": {}
    }

    def test_inconsistent_objects(self, hge_ctx):
        with open(self.dir() + "/test.yaml") as c:
            test = yaml.load(c)

        # setup
        st_code, resp = hge_ctx.v1q(json.loads(json.dumps(test['setup'])))
        assert st_code == 200, resp

        # exec sql to cause inconsistentancy
        sql_res = hge_ctx.sql(test['sql'])

        # reload metadata
        st_code, resp = hge_ctx.v1q(q=self.reload_metadata)
        assert st_code == 200, resp

        # fetch inconsistent objects
        st_code, resp = hge_ctx.v1q(q=self.get_inconsistent_metadata)
        assert st_code == 200, resp
        incons_objs_test = test['inconsistent_objects']
        incons_objs_resp = resp['inconsistent_objects']

        assert resp['is_consistent'] == False, resp
        assert json_ordered(incons_objs_resp) == json_ordered(incons_objs_test), yaml.dump({
            'response': resp,
            'expected': incons_objs_test,
            'diff': jsondiff.diff(incons_objs_test, resp)
        })

        # export metadata
        st_code, export = hge_ctx.v1q(q=self.export_metadata)
        assert st_code == 200, export

        # apply metadata
        st_code, resp = hge_ctx.v1q(
            q={
                "type": "replace_metadata",
                "args": export
            }
        )
        assert st_code == 400, resp

        # drop inconsistent objects
        st_code, resp = hge_ctx.v1q(q=self.drop_inconsistent_metadata)
        assert st_code == 200, resp

        # reload metadata
        st_code, resp = hge_ctx.v1q(q=self.reload_metadata)
        assert st_code == 200, resp

        # fetch inconsistent objects
        st_code, resp = hge_ctx.v1q(q=self.get_inconsistent_metadata)
        assert st_code == 200, resp

        assert resp['is_consistent'] == True, resp
        assert len(resp['inconsistent_objects']) == 0, resp

        # teardown
        st_code, resp = hge_ctx.v1q(json.loads(json.dumps(test['teardown'])))
        assert st_code == 200, resp

    @classmethod
    def dir(cls):
        return 'queries/inconsistent_objects'
