import { renderHook } from '@testing-library/react-hooks';
import { wrapper } from '../../../../../hooks/__tests__/common/decorator';
import { useRows } from '.';
import { server, postgresTableMockData } from '../mocks/handlers.mock';
import { UseRowsPropType } from './useRows';

describe('useRemoveAgent tests: ', () => {
  beforeAll(() => {
    server.listen();
  });
  afterAll(() => {
    server.close();
  });

  it('returns table data for a postgres table', async () => {
    const props: UseRowsPropType = {
      dataSourceName: 'chinook',
      table: { name: 'Album', schema: 'public' },
      options: {
        limit: 10,
        where: { $and: [{ AlbumId: { $gt: 4 } }] },
        order_by: [{ column: 'Title', type: 'desc' }],
        offset: 15,
      },
    };
    const { result, waitFor } = renderHook(() => useRows(props), { wrapper });
    await waitFor(() => result.current.isSuccess);
    expect(result.current.data).toEqual(postgresTableMockData);
  });
});
