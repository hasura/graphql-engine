import { setupServer } from 'msw/node';
import { renderHook } from '@testing-library/react-hooks';
import { handlers } from '../../../../mocks/metadata.mock';
import { useAddOperationsToQueryCollection } from '.';
import { wrapper } from '../../../../hooks/__tests__/common/decorator';

const server = setupServer();

beforeAll(() => server.listen());
afterAll(() => server.close());

describe('useAddOperationsToQueryCollection', () => {
  beforeEach(() => {
    server.use(...handlers({ url: '' }));
  });

  test('When useAddOperationsToQueryCollection is used with a valid QueryCollection Then it should call the API with correct payload', async () => {
    const { waitForValueToChange, result }: any = renderHook(
      () => useAddOperationsToQueryCollection(),
      { wrapper }
    );

    await result.current.addOperationToQueryCollection('other_queries', [
      {
        name: 'MyQuery7',
        query: 'query MyQuery7 { user { email name}}',
      },
    ]);

    await waitForValueToChange(() => result.current.isSuccess);
    expect(result.current.isSuccess).toBe(true);
  });
});
