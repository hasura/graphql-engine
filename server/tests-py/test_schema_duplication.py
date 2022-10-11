#!/usrbin/env python3

import pytest

from validate import check_query_f

@pytest.mark.usefixtures('actions_fixture', 'per_method_tests_db_state')
class TestSchemaDuplication:

    @classmethod
    def dir(cls):
        return "queries/schema/duplication/"

    def test_create_action_followed_by_track_table(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "create_action_and_track_table_fail.yaml")

    def test_track_table_followed_by_create_action(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "track_table_and_create_action_fail.yaml")

    def test_track_table_with_conflicting_custom_root_node_names(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'track_table_with_conflicting_custom_root_node_names_fail.yaml')
