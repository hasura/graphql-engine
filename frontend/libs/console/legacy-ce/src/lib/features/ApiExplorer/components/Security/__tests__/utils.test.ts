import { getLimitsforRole } from './../utils';
import { Metadata } from '@hasura/shared/types';

const metadata: Metadata = {
  resource_version: 1,
  metadata: {
    version: 3,
    sources: [],
    inherited_roles: [],
    api_limits: {
      depth_limit: {
        state: 'enabled',
        per_role: {
          user: 4,
          new_role: 2,
          test_role: 5,
          editor: 3,
        },
        global: 5,
      },
      disabled: false,
      node_limit: {
        state: 'enabled',
        per_role: {
          user: 5,
          new_role: 2,
          test_role: 6,
          editor: 3,
        },
        global: 6,
      },
    },
    graphql_schema_introspection: {
      disabled_for_roles: ['new_role'],
    },
  },
};

describe('Gets limits for a role', () => {
  test('Works for existing roles', () => {
    const role_user = getLimitsforRole(metadata)('user');
    expect(role_user).toMatchSnapshot();

    const role_new_role = getLimitsforRole(metadata)('new_role');
    expect(role_new_role).toMatchSnapshot();

    const role_test_role = getLimitsforRole(metadata)('test_role');
    expect(role_test_role).toMatchSnapshot();

    const role_editor = getLimitsforRole(metadata)('editor');
    expect(role_editor).toMatchSnapshot();
  });
});
