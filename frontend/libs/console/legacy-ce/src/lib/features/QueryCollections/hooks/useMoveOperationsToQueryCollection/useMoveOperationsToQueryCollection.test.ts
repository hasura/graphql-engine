import { setupServer } from 'msw/node';
import { renderHook } from '@testing-library/react-hooks';
import { handlers } from '../../../..//mocks/metadata.mock';
import { useMoveOperationsToQueryCollection } from '.';
import { wrapper } from '../../../../hooks/__tests__/common/decorator';

const server = setupServer();

beforeAll(() => server.listen());
afterAll(() => server.close());

describe('useMoveOperationsToQueryCollection', () => {
  beforeEach(() => {
    server.use(...handlers({ url: '' }));
  });

  test('When useMoveOperationsToQueryCollection is used with a valid input Then it should call the API with correct payload', async () => {
    const { waitForValueToChange, result }: any = renderHook(
      () => useMoveOperationsToQueryCollection(),
      { wrapper }
    );

    await result.current.moveOperationToQueryCollection(
      'allowed-queries',
      'other_queries',
      [
        {
          name: 'MyQuery',
          query: 'query NewMyQuery { user { email name}}',
        },
      ]
    );

    await waitForValueToChange(() => result.current.isSuccess);
    expect(result.current.isSuccess).toBe(true);
  });
});
