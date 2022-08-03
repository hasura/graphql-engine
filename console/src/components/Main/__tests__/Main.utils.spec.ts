import type { IsBlockActiveArgs } from '../Main.utils';

import { isBlockActive } from '../Main.utils';

describe('isBlockActive', () => {
  it('When the user navigates the default home block, then returns true for the default block', () => {
    const args: IsBlockActiveArgs = {
      blockPath: '/api/api-explorer',
      isDefaultBlock: true,
      pathname: '/',
    };
    expect(isBlockActive(args)).toBe(true);
  });

  it('When the user navigates the current block, then returns true', () => {
    const args: IsBlockActiveArgs = {
      blockPath: '/data/default',
      isDefaultBlock: false,
      pathname: '/data/default',
    };
    expect(isBlockActive(args)).toBe(true);
  });
});
