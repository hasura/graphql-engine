import { setupServer } from 'msw/node';
import { renderHook } from '@testing-library/react-hooks';
import { handlers } from '../../../../mocks/metadata.mock';
import { useOperationsFromQueryCollection } from './useOperationsFromQueryCollection';
import { wrapper } from '../../../../hooks/__tests__/common/decorator';

const server = setupServer();

beforeAll(() => server.listen());
afterAll(() => server.close());

describe('useOperationsFromQueryCollection with valid data', () => {
  beforeEach(() => {
    server.use(...handlers({ url: '' }));
  });
  test('When useOperationsFromQueryCollection hook is called with query collection Name, then a valid list of operations are returned', async () => {
    const { result, waitForValueToChange } = renderHook(
      () => useOperationsFromQueryCollection('allowed-queries'),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.isSuccess);

    const operations = result.current.data!;

    expect(operations).toHaveLength(3);
  });
});

describe('useOperationsFromQueryCollection with no query collections', () => {
  beforeEach(() => {
    server.use(...handlers({ url: '' }));
  });
  test('When useOperationsFromQueryCollection is called with an invalid query collection, then empty array should be returned', async () => {
    const { result, waitForValueToChange } = renderHook(
      () => useOperationsFromQueryCollection('allowed-queries'),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.isSuccess);

    const operations = result.current.data!;

    expect(operations[0].query)
      .toEqual(`mutation update_user_by_pk($id: Int!, $object: user_set_input!) {
  update_user_by_pk(pk_columns: {id: $id}, _set: $object) {
    address
    bool
    count
    date
    email
    id
    name
    uuid
  }
}
`);

    expect(operations).toHaveLength(3);
  });
});
