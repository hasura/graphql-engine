import { setupServer } from 'msw/node';
import { renderHook } from '@testing-library/react-hooks';
import { handlers } from '../../../../mocks/metadata.mock';
import { useRemoveFromAllowList } from './useRemoveFromAllowList';
import { wrapper } from '../../../../hooks/__tests__/common/decorator';

const server = setupServer();

beforeAll(() => server.listen());
afterAll(() => server.close());

describe('useRemoveFromAllowList', () => {
  beforeEach(() => {
    server.use(...handlers({ url: '' }));
  });

  test('should work correctly when deleting an existing collection', async () => {
    const { waitForValueToChange, result }: any = renderHook(
      () => useRemoveFromAllowList(),
      { wrapper }
    );

    await result.current.removeFromAllowList('allowed-queries');

    await waitForValueToChange(() => result.current.isSuccess);
    expect(result.current.isSuccess).toBe(true);
  });

  test('should go in error state when deleting a non existing collection', async () => {
    const { waitForValueToChange, result }: any = renderHook(
      () => useRemoveFromAllowList(),
      { wrapper }
    );

    await result.current.removeFromAllowList('not-existing');

    await waitForValueToChange(() => result.current.error);
    expect(result.current.error).toBeDefined();
  });
});
