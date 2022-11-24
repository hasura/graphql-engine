import json
import jsondiff
from ruamel.yaml import YAML

yaml=YAML(typ='safe', pure=True)

def load_yaml(path):
    with open(path) as f:
        return json.loads(json.dumps(yaml.load(f)))

class TestInconsistentObjects():

    reload_metadata = {
        "type": "reload_metadata",
        "args": {},
    }
    drop_inconsistent_metadata = {
        "type": "drop_inconsistent_metadata",
        "args": {},
    }
    export_metadata = {
        "type": "export_metadata",
        "args": {},
    }

    def test_inconsistent_objects(self, hge_ctx, source_backend):
        setup = load_yaml(self.dir() + "/setup.yaml")
        expected_inconsistent_objects = load_yaml(self.dir() + "/expectation.yaml")
        teardown = load_yaml(self.dir() + "/teardown.yaml")

        hge_ctx.v1q(setup)

        try:
            # exec sql to cause inconsistentancy
            # TODO: remove once parallelization work is completed
            #       `source_backend` will no longer be optional
            if source_backend:
                with source_backend.engine.connect() as connection:
                    connection.execute('drop table article')
            else:
                # this only works when the metadata database and the source database are the same
                hge_ctx.sql('drop table article')

            # reload metadata
            resp = hge_ctx.v1q(q=self.reload_metadata)
            # check inconsistent objects
            actual_inconsistent_objects = resp['inconsistent_objects']

            assert resp['is_consistent'] == False, resp
            assert actual_inconsistent_objects == expected_inconsistent_objects, yaml.dump({
                'response': actual_inconsistent_objects,
                'expected': expected_inconsistent_objects,
                'diff': jsondiff.diff(expected_inconsistent_objects, actual_inconsistent_objects)
            })

            # export metadata
            export = hge_ctx.v1q(q=self.export_metadata)

            # apply metadata
            hge_ctx.v1q(
                q={
                    "type": "replace_metadata",
                    "args": export,
                },
                expected_status_code = 400,
            )

        finally:
            # drop inconsistent objects
            hge_ctx.v1q(q=self.drop_inconsistent_metadata)

            # reload metadata
            resp = hge_ctx.v1q(q=self.reload_metadata)
            # check inconsistent objects
            assert resp['is_consistent'] == True, resp

            hge_ctx.v1q(teardown)

    @classmethod
    def dir(cls):
        return 'queries/inconsistent_objects'
