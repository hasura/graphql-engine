import pytest
import json
import jsondiff
from ruamel.yaml import YAML

yaml=YAML(typ='safe', pure=True)

class TestInconsistentObjects():

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
        resp = hge_ctx.v1q(json.loads(json.dumps(test['setup'])))

        try:
            # exec sql to cause inconsistentancy
            hge_ctx.sql(test['sql'])

            # reload metadata
            resp = hge_ctx.v1q(q=self.reload_metadata)
            # check inconsistent objects
            incons_objs_test = test['inconsistent_objects']
            incons_objs_resp = resp['inconsistent_objects']

            assert resp['is_consistent'] == False, resp
            assert incons_objs_resp == incons_objs_test, yaml.dump({
                'response': incons_objs_resp,
                'expected': incons_objs_test,
                'diff': jsondiff.diff(incons_objs_test, incons_objs_resp)
            })

            # export metadata
            export = hge_ctx.v1q(q=self.export_metadata)

            # apply metadata
            hge_ctx.v1q(
                q={
                    "type": "replace_metadata",
                    "args": export
                },
                expected_status_code = 400
            )

        finally:
            # drop inconsistent objects
            hge_ctx.v1q(q=self.drop_inconsistent_metadata)

            # reload metadata
            resp = hge_ctx.v1q(q=self.reload_metadata)
            # check inconsistent objects
            assert resp['is_consistent'] == True, resp

            # teardown
            hge_ctx.v1q(json.loads(json.dumps(test['teardown'])))

    @classmethod
    def dir(cls):
        return 'queries/inconsistent_objects'
