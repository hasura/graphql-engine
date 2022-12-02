import { setupServer } from 'msw/node';
import { renderHook } from '@testing-library/react-hooks';
import { handlers } from '../../../../mocks/metadata.mock';
import { useEditOperationInQueryCollection } from '.';
import { wrapper } from '../../../../hooks/__tests__/common/decorator';

const server = setupServer();

beforeAll(() => server.listen());
afterAll(() => server.close());

describe('useEditOperationInQueryCollection', () => {
  beforeEach(() => {
    server.use(...handlers({ url: '' }));
  });

  test('When useEditOperationInQueryCollection is used with a valid input Then it should call the API with correct payload', async () => {
    const { waitForValueToChange, result }: any = renderHook(
      () => useEditOperationInQueryCollection(),
      { wrapper }
    );

    await result.current.editOperationInQueryCollection(
      'allowed-queries',
      'MyQuery',
      [
        {
          name: 'NewMyQuery',
          query: 'query NewMyQuery { user { email name}}',
        },
      ]
    );

    await waitForValueToChange(() => result.current.isSuccess);
    expect(result.current.isSuccess).toBe(true);
  });
});
