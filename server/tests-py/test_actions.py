#!/usr/bin/env python3

import pytest

from validate import check_query_f
from super_classes import DefaultTestQueries

"""
TODO:-
1. Actions metadata
2. Actions Asynchronous (query with action_id of an action created by other user)
3. Actions Permissions
"""

class TestActionsSync(DefaultTestQueries):

    @classmethod
    def dir(cls):
        return 'queries/actions/sync'

    def test_create_user_fail(self, hge_ctx, actions_webhook):
        check_query_f(hge_ctx, self.dir() + '/create_user_fail.yaml')

    def test_create_user_success(self, hge_ctx, actions_webhook):
        check_query_f(hge_ctx, self.dir() + '/create_user_success.yaml')
