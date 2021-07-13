import { api_limits } from './fixtures/input';
import { updateAPILimitsQuery, removeAPILimitsQuery } from '../utils';

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
