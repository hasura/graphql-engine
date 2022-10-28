import { setupServer } from 'msw/node';
import { renderHook } from '@testing-library/react-hooks';
import { handlers } from './mocks/handlers.mock';
import { useAddOperationsToQueryCollection } from '.';
import { wrapper } from '../../../../hooks/__tests__/common/decorator';

const server = setupServer();

beforeAll(() => server.listen());
afterAll(() => server.close());

describe('useAddOperationsToQueryCollection', () => {
  beforeEach(() => {
    server.use(...handlers(1, ''));
  });

  test('When useAddOperationsToQueryCollection is used with a valid QueryCollection Then it should call the API with correct payload', async () => {
    const { waitForValueToChange, result }: any = renderHook(
      () => useAddOperationsToQueryCollection(),
      { wrapper }
    );

    await result.current.addOperationToQueryCollection('testCollection', [
      {
        name: 'MyQuery33',
        query: 'query MyQuery { user { email name}}',
      },
    ]);

    await waitForValueToChange(() => result.current.isSuccess);
    expect(result.current.isSuccess).toBe(true);
  });
});
