import { TableColumn } from '../../DataSource';
import { adaptFormValuesToQuery } from './FiltersSectionContainer.utils';
import { FiltersAndSortFormValues, UserQuery } from './types';

describe('adaptFormValuesToQuery', () => {
  it('adapts the form values into a query', () => {
    const input: FiltersAndSortFormValues = {
      filter: [
        { column: 'text', operator: '$eq', value: 'aaaa' },
        { column: 'id', operator: '$eq', value: '1' },
      ],
      sort: [{ column: 'id', order: 'asc' }],
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
