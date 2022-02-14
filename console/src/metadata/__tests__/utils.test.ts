import { api_limits, introspection_options } from './fixtures/input';
import {
  updateAPILimitsQuery,
  removeAPILimitsQuery,
  updateIntrospectionOptionsQuery,
} from '../utils';

describe('Testing API limits', () => {
  it('only depth limit', () => {
    const res = updateAPILimitsQuery({
      existingAPILimits: api_limits.only_depth_limit.old_state,
      newAPILimits: api_limits.only_depth_limit.new_state,
    });
    expect(res).toMatchSnapshot();
  });

  it('only node limit', () => {
    const res = updateAPILimitsQuery({
      existingAPILimits: api_limits.only_node_limit.old_state,
      newAPILimits: api_limits.only_node_limit.new_state,
    });
    expect(res).toMatchSnapshot();
  });

  it('only depth limit with per role', () => {
    const res = updateAPILimitsQuery({
      existingAPILimits: api_limits.only_depth_limit_with_per_role.old_state,
      newAPILimits: api_limits.only_depth_limit_with_per_role.new_state,
    });
    expect(res).toMatchSnapshot();
  });

  it('only node limit with per role', () => {
    const res = updateAPILimitsQuery({
      existingAPILimits: api_limits.only_node_limit_with_per_role.old_state,
      newAPILimits: api_limits.only_node_limit_with_per_role.new_state,
    });
    expect(res).toMatchSnapshot();
  });

  it('node limit and depth limits', () => {
    const res = updateAPILimitsQuery({
      existingAPILimits: api_limits.node_and_depth_limits.old_state,
      newAPILimits: api_limits.node_and_depth_limits.new_state,
    });
    expect(res).toMatchSnapshot();
  });
});

describe('Testing Remove API Settings', () => {
  it('removes settings for a role', () => {
    const res = removeAPILimitsQuery({
      existingAPILimits: api_limits.node_and_depth_limits.new_state,
      role: 'role1',
    });
    expect(res).toMatchSnapshot();
  });
  it("remains the same when role doesn't exist", () => {
    const res = removeAPILimitsQuery({
      existingAPILimits: api_limits.only_depth_limit.new_state,
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
      Object {
        "args": Object {
          "disabled_for_roles": Array [],
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
      Object {
        "args": Object {
          "disabled_for_roles": Array [
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
      Object {
        "args": Object {
          "disabled_for_roles": Array [
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
      Object {
        "args": Object {
          "disabled_for_roles": Array [
            "existing_role",
            "test_role_1",
          ],
        },
        "type": "set_graphql_schema_introspection_options",
      }
    `);
  });
});
