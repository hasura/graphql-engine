import { isBlockActive } from '../Main.utils';

describe('isBlockActive', () => {
  it.each([
    {
      blockPath: '/api/api-explorer',
      isDefaultBlock: true,
      pathname: '/',
      expected: true,
    },
    {
      blockPath: '/data/default',
      isDefaultBlock: false,
      pathname: '/data/default',
      expected: true,
    },
  ])('for %o', ({ blockPath, isDefaultBlock, pathname, expected }) => {
    const currentActiveBlock = isBlockActive({
      blockPath,
      isDefaultBlock,
      pathname,
    });
    expect(currentActiveBlock).toBe(expected);
  });
});
