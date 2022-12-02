import { setupServer } from 'msw/node';
import { renderHook } from '@testing-library/react-hooks';
import { handlers } from '../../../../mocks/metadata.mock';
import { useQueryCollections } from './useQueryCollections';
import { wrapper } from '../../../../hooks/__tests__/common/decorator';

const server = setupServer();

beforeAll(() => server.listen());
afterAll(() => server.close());

describe('useQueryCollections', () => {
  beforeEach(() => {
    server.use(...handlers({ url: '' }));
  });
  test('should return the query collection when called without errors', async () => {
    const { result, waitForValueToChange } = renderHook(
      () => useQueryCollections(),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.isSuccess);

    const queryCollections = result.current.data;

    expect(queryCollections).toHaveLength(2);
    expect(queryCollections?.[0]?.name).toEqual('allowed-queries');
    expect(queryCollections?.[1]?.name).toEqual('other_queries');
  });
});
