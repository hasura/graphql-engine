import { setupServer } from 'msw/node';
import { renderHook } from '@testing-library/react-hooks';
import { handlers } from '../../../../mocks/metadata.mock';
import { useCreateQueryCollection } from './useCreateQueryCollection';
import { wrapper } from '../../../../hooks/__tests__/common/decorator';

const server = setupServer();

beforeAll(() => server.listen());
afterAll(() => server.close());

describe('useCreateQueryCollections', () => {
  beforeEach(() => {
    server.use(...handlers({ url: '' }));
  });

  test('should work correctly when creating a non-existing collection', async () => {
    const { waitForValueToChange, result }: any = renderHook(
      () => useCreateQueryCollection(),
      { wrapper }
    );

    await result.current.createQueryCollection('new-collection');

    await waitForValueToChange(() => result.current.isSuccess);
    expect(result.current.isSuccess).toBe(true);
  });

  test('should go in error state when creating an existing collection', async () => {
    const { waitForValueToChange, result }: any = renderHook(
      () => useCreateQueryCollection(),
      { wrapper }
    );

    await result.current.createQueryCollection('allowed-queries');

    await waitForValueToChange(() => result.current.error);
    expect(result.current.error).toBeDefined();
  });
});
