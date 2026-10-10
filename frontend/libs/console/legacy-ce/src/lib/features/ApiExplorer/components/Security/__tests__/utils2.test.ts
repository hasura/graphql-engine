import {
  updateAPILimitsQuery,
  removeAPILimitsQuery,
  updateIntrospectionOptionsQuery,
} from '@hasura/metadata/api';
import { api_limits, introspection_options } from './fixtures/input';

describe('Testing API limits', () => {
  it('only depth limit', () => {
    const res = updateAPILimitsQuery(api_limits.only_depth_limit);
    expect(res).toMatchSnapshot();
  });

  it('only node limit', () => {
    const res = updateAPILimitsQuery(api_limits.only_node_limit);
    expect(res).toMatchSnapshot();
  });

  it('only batch limit', () => {
    const res = updateAPILimitsQuery(api_limits.only_batch_limit);
    expect(res).toMatchSnapshot();
  });

  it('only depth limit with per role', () => {
    const res = updateAPILimitsQuery(api_limits.only_depth_limit_with_per_role);
    expect(res).toMatchSnapshot();
  });

  it('only node limit with per role', () => {
    const res = updateAPILimitsQuery(api_limits.only_node_limit_with_per_role);
    expect(res).toMatchSnapshot();
  });

  it('only batch limit with per role', () => {
    const res = updateAPILimitsQuery(api_limits.only_batch_limit_with_per_role);
    expect(res).toMatchSnapshot();
  });

  it('node limit, batch limit and depth limit', () => {
    const res = updateAPILimitsQuery(api_limits.node_depth_and_batch_limits);
    expect(res).toMatchSnapshot();
  });
});

describe('Testing Remove API Settings', () => {
  it('removes settings for a role', () => {
    const res = removeAPILimitsQuery({
      existingAPILimits:
        api_limits.node_depth_and_batch_limits.existingAPILimits,
      role: 'role1',
    });
    expect(res).toMatchSnapshot();
  });
  it("remains the same when role doesn't exist", () => {
    const res = removeAPILimitsQuery({
      existingAPILimits: api_limits.only_depth_limit.existingAPILimits,
      role: 'role1',
    });
    expect(res).toMatchSnapshot();
  });
});

describe('Testing Introspection Utils', () => {
  it('enabling for a user role', () => {
    const res = updateIntrospectionOptionsQuery({
      existingOptions: introspection_options.enable_for_role.old_state,
      roleName: introspection_options.enable_for_role.role,
      introspectionIsDisabled:
        introspection_options.enable_for_role.introspection_is_disabled,
    });
    expect(res).toMatchInlineSnapshot(`
      {
        "args": {
          "disabled_for_roles": [],
        },
        "type": "set_graphql_schema_introspection_options",
      }
    `);
  });

  it('enabling for a user role when there are roles already disabled', () => {
    const res = updateIntrospectionOptionsQuery({
      existingOptions:
        introspection_options.state_is_present_and_enable_for_role.old_state,
      roleName: introspection_options.state_is_present_and_enable_for_role.role,
      introspectionIsDisabled:
        introspection_options.state_is_present_and_enable_for_role
          .introspection_is_disabled,
    });
    expect(res).toMatchInlineSnapshot(`
      {
        "args": {
          "disabled_for_roles": [
            "existing_role",
          ],
        },
        "type": "set_graphql_schema_introspection_options",
      }
    `);
  });

  it('disabling for a user role', () => {
    const res = updateIntrospectionOptionsQuery({
      existingOptions: introspection_options.disable_for_role.old_state,
      roleName: introspection_options.disable_for_role.role,
      introspectionIsDisabled:
        introspection_options.disable_for_role.introspection_is_disabled,
    });
    expect(res).toMatchInlineSnapshot(`
      {
        "args": {
          "disabled_for_roles": [
            "test_role",
          ],
        },
        "type": "set_graphql_schema_introspection_options",
      }
    `);
  });

  it('disabling for a user role when there are roles already disabled', () => {
    const res = updateIntrospectionOptionsQuery({
      existingOptions:
        introspection_options.state_is_present_and_disable_for_role.old_state,
      roleName:
        introspection_options.state_is_present_and_disable_for_role.role,
      introspectionIsDisabled:
        introspection_options.state_is_present_and_disable_for_role
          .introspection_is_disabled,
    });
    expect(res).toMatchInlineSnapshot(`
      {
        "args": {
          "disabled_for_roles": [
            "existing_role",
            "test_role_1",
          ],
        },
        "type": "set_graphql_schema_introspection_options",
      }
    `);
  });
});
