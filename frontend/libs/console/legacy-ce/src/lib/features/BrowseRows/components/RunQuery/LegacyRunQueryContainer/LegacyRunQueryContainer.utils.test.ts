import { TableColumn } from '../../../../DataSource';
import {
  adaptFormValuesToQuery,
  filterValidUserQuery,
} from './LegacyRunQueryContainer.utils';
import { FiltersAndSortFormValues, UserQuery } from '../types';

describe('adaptFormValuesToQuery', () => {
  it('adapts the form values into a query', () => {
    const input: FiltersAndSortFormValues = {
      filter: [
        { column: 'text', operator: '$eq', value: 'aaaa' },
        { column: 'id', operator: '$eq', value: '1' },
      ],
      sort: [{ column: 'id', type: 'asc' }],
    };
    const expected: UserQuery = {
      where: {
        $and: [
          {
            text: {
              $eq: 'aaaa',
            },
          },
          {
            id: {
              $eq: 1,
            },
          },
        ],
      },
      order_by: [
        {
          column: 'id',
          type: 'asc',
          nulls: 'last',
        },
      ],
    };

    const columnDataTypes: TableColumn[] = [
      {
        name: 'id',
        dataType: 'integer',
      },
      {
        name: 'text',
        dataType: 'text',
      },
    ];
    expect(adaptFormValuesToQuery(input, columnDataTypes)).toEqual(expected);
  });

  it('converts the $in values into array', () => {
    const input: FiltersAndSortFormValues = {
      filter: [{ column: 'id', operator: '$in', value: '[1, 2]' }],
      sort: [],
    };
    const expected: UserQuery = {
      where: {
        $and: [
          {
            id: {
              $in: [1, 2],
            },
          },
        ],
      },
      order_by: [],
    };

    const columnDataTypes: TableColumn[] = [
      {
        name: 'id',
        dataType: 'integer',
      },
    ];
    expect(adaptFormValuesToQuery(input, columnDataTypes)).toEqual(expected);
  });
});

describe('filterValidUserQuery', () => {
  it('removes empty and conditions', () => {
    const userQuery: UserQuery = {
      where: { $and: [{ '': { '': '' } }] },
      order_by: [{ column: '', type: '--', nulls: 'last' }],
    };

    const expected: UserQuery = {
      where: { $and: [] },
      order_by: [],
    };
    expect(filterValidUserQuery(userQuery)).toEqual(expected);
  });

  it('removes empty and conditions and returns valid conditions', () => {
    const userQuery: UserQuery = {
      where: { $and: [{ '': { '': '' } }, { name: { $eq: 'foo' } }] },
      order_by: [
        { column: '', type: '--', nulls: 'last' },
        { column: 'name', type: 'desc', nulls: 'last' },
      ],
    };

    const expected: UserQuery = {
      where: { $and: [{ name: { $eq: 'foo' } }] },
      order_by: [{ column: 'name', type: 'desc', nulls: 'last' }],
    };
    expect(filterValidUserQuery(userQuery)).toEqual(expected);
  });

  it('returns valid conditions', () => {
    const userQuery: UserQuery = {
      where: { $and: [{ name: { $eq: 'foo' } }] },
      order_by: [{ column: 'name', type: 'desc', nulls: 'last' }],
    };

    const expected: UserQuery = {
      where: { $and: [{ name: { $eq: 'foo' } }] },
      order_by: [{ column: 'name', type: 'desc', nulls: 'last' }],
    };
    expect(filterValidUserQuery(userQuery)).toEqual(expected);
  });
});
