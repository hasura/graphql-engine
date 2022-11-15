import { setupServer } from 'msw/node';
import { renderHook } from '@testing-library/react-hooks';
import { handlers } from '../../../../mocks/metadata.mock';
import { useAddToAllowList } from './useAddToAllowList';
import { wrapper } from '../../../../hooks/__tests__/common/decorator';

const server = setupServer();

beforeAll(() => server.listen());
afterAll(() => server.close());

describe('useAddToAllowList', () => {
  beforeEach(() => {
    server.use(...handlers({ url: '' }));
  });

  test('should work correctly when adding an existing collection', async () => {
    const { waitForValueToChange, result }: any = renderHook(
      () => useAddToAllowList(),
      { wrapper }
    );

    await result.current.addToAllowList('not-existing');

    await waitForValueToChange(() => result.current.isSuccess);
    expect(result.current.isSuccess).toBe(true);
  });

  test('should go in error state when adding a non existing collection', async () => {
    const { waitForValueToChange, result }: any = renderHook(
      () => useAddToAllowList(),
      { wrapper }
    );

    await result.current.addToAllowList('allowed-queries');

    await waitForValueToChange(() => result.current.error);
    expect(result.current.error).toBeDefined();
  });
});
