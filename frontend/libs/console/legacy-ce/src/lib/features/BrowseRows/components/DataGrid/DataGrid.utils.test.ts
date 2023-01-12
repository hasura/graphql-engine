import { TableRow, WhereClause } from '../../../../features/DataSource';
import {
  adaptSelectedRowIdsToWhereClause,
  AdaptSelectedRowIdsToWhereClauseArgs,
} from './DataGrid.utils';

describe('adaptSelectedRowIdsToWhereClause', () => {
  it('adapts selected rows', () => {
    const rowsId: AdaptSelectedRowIdsToWhereClauseArgs['rowsId'] = {
      0: true,
      3: true,
    };

    const rows: TableRow[] = [
      {
        ArtistId: 1,
        Name: 'The Beatles',
      },
      {
        ArtistId: 2,
        Name: 'AC/DC',
      },
      {
        ArtistId: 3,
        Name: 'Aerosmith',
      },
      {
        ArtistId: 4,
        Name: 'Audioslave',
      },
      {
        ArtistId: 5,
        Name: '	Billy Cobham',
      },
    ];

    const primaryKeys = ['ArtistId'];

    const expected: WhereClause[] = [
      {
        ArtistId: { _in: [1, 4] },
      },
    ];
    expect(
      adaptSelectedRowIdsToWhereClause({
        rowsId,
        rows,
        primaryKeys,
      })
    ).toEqual(expected);
  });
});
