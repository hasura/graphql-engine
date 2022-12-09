import { setupServer } from 'msw/node';
import { renderHook } from '@testing-library/react-hooks';
import { handlers } from '../../../../mocks/metadata.mock';
import { useRenameQueryCollection } from './useRenameQueryCollection';
import { wrapper } from '../../../../hooks/__tests__/common/decorator';

const server = setupServer();

beforeAll(() => server.listen());
afterAll(() => server.close());

describe('useRenameQueryCollection', () => {
  beforeEach(() => {
    server.use(...handlers({ url: '' }));
  });

  test('should work correctly when renaming an existing collection', async () => {
    const { waitForValueToChange, result }: any = renderHook(
      () => useRenameQueryCollection(),
      { wrapper }
    );

    await result.current.renameQueryCollection('other_queries', 'new-name');

    await waitForValueToChange(() => result.current.isSuccess);
    expect(result.current.isSuccess).toBe(true);
  });

  test('should go in error state when renaming a non-existing collection', async () => {
    const { waitForValueToChange, result }: any = renderHook(
      () => useRenameQueryCollection(),
      { wrapper }
    );

    await result.current.renameQueryCollection('not-existing', 'new-name');

    await waitForValueToChange(() => result.current.error);
    expect(result.current.error).toBeDefined();
  });
});
