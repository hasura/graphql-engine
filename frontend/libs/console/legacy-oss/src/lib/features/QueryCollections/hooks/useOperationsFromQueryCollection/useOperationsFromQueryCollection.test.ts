import { setupServer } from 'msw/node';
import { rest } from 'msw';
import { renderHook } from '@testing-library/react-hooks';
import { metadata, metadata_with_no_query_collections } from './mocks/metadata';
import { useOperationsFromQueryCollection } from './useOperationsFromQueryCollection';
import { wrapper } from '../../../../hooks/__tests__/common/decorator';

const server = setupServer();

beforeAll(() => server.listen());
afterAll(() => server.close());

describe('useOperationsFromQueryCollection with valid data', () => {
  beforeEach(() => {
    server.use(
      rest.post('/v1/metadata', (req, res, ctx) =>
        res(ctx.status(200), ctx.json(metadata))
      )
    );
  });
  test('When useOperationsFromQueryCollection hook is called with query collection Name, then a valid list of operations are returned', async () => {
    const { result, waitForValueToChange } = renderHook(
      () => useOperationsFromQueryCollection('allowed-queries'),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.isSuccess);

    const operations = result.current.data!;

    expect(operations).toHaveLength(1);
    expect(operations[0].name).toEqual('MyQuery');
    expect(operations[0].query).toEqual(
      'query MyQuery {\n  user {\n    id\n  }\n}\n'
    );
  });
});

describe('useOperationsFromQueryCollection with no query collections', () => {
  beforeEach(() => {
    server.use(
      rest.post('/v1/metadata', (req, res, ctx) =>
        res(ctx.status(200), ctx.json(metadata_with_no_query_collections))
      )
    );
  });
  test('When useOperationsFromQueryCollection is called with an invalid query collection, then empty array should be returned', async () => {
    const { result, waitForValueToChange } = renderHook(
      () => useOperationsFromQueryCollection('allowed-queries'),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.isSuccess);

    const operations = result.current.data!;

    expect(operations).toHaveLength(0);
  });
});
