import { setupServer } from 'msw/node';
import { renderHook } from '@testing-library/react-hooks';
import { handlers } from '../../../../mocks/metadata.mock';
import { useRemoveOperationsFromQueryCollection } from '.';
import { wrapper } from '../../../../hooks/__tests__/common/decorator';

const server = setupServer();

beforeAll(() => server.listen());
afterAll(() => server.close());

describe('useRemoveOperationsFromQueryCollection', () => {
  beforeEach(() => {
    server.use(...handlers({ url: '' }));
  });

  test('When useRemoveOperationsFromQueryCollection is used with a valid QueryCollection Then it should call the API with correct payload', async () => {
    const { waitForValueToChange, result }: any = renderHook(
      () => useRemoveOperationsFromQueryCollection(),
      { wrapper }
    );

    await result.current.removeOperationsFromQueryCollection(
      'allowed-queries',
      [
        {
          name: 'MyQuery',
          query: 'query MyQuery { user { email name}}',
        },
      ]
    );

    await waitForValueToChange(() => result.current.isSuccess);
    expect(result.current.isSuccess).toBe(true);
  });
});
