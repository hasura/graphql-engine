import { TableColumn } from '../../../../DataSource';
import {
  adaptFormValuesToQuery,
  filterValidUserQuery,
  convertUserQueryToFiltersAndSortFormValues,
} from './LegacyRunQueryContainer.utils';
import { FiltersAndSortFormValues, UserQuery } from '../types';

describe('adaptFormValuesToQuery', () => {
  it('adapts the form values into a query', () => {
    const input: FiltersAndSortFormValues = {
      filters: [
        { column: 'text', operator: '$eq', value: 'aaaa' },
        { column: 'id', operator: '$eq', value: '1' },
      ],
      sorts: [{ column: 'id', type: 'asc' }],
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
        dataType: 'number',
        consoleDataType: 'number',
      },
      {
        name: 'text',
        dataType: 'string',
        consoleDataType: 'string',
      },
    ];
    expect(adaptFormValuesToQuery(input, columnDataTypes)).toEqual(expected);
  });

  it('converts the $in values into array', () => {
    const input: FiltersAndSortFormValues = {
      filters: [{ column: 'id', operator: '$in', value: '[1, 2]' }],
      sorts: [],
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
        dataType: 'number',
        consoleDataType: 'number',
      },
    ];
    expect(adaptFormValuesToQuery(input, columnDataTypes)).toEqual(expected);
  });

  it('converts false boolean values', () => {
    const input: FiltersAndSortFormValues = {
      filters: [{ column: 'idDeleted', operator: '$eq', value: 'false' }],
      sorts: [],
    };
    const expected: UserQuery = {
      where: {
        $and: [
          {
            idDeleted: {
              $eq: false,
            },
          },
        ],
      },
      order_by: [],
    };

    const columnDataTypes: TableColumn[] = [
      {
        name: 'idDeleted',
        dataType: 'bool',
        consoleDataType: 'boolean',
      },
    ];
    expect(adaptFormValuesToQuery(input, columnDataTypes)).toEqual(expected);
  });

  it('converts true boolean values', () => {
    const input: FiltersAndSortFormValues = {
      filters: [{ column: 'idDeleted', operator: '$eq', value: 'true' }],
      sorts: [],
    };
    const expected: UserQuery = {
      where: {
        $and: [
          {
            idDeleted: {
              $eq: true,
            },
          },
        ],
      },
      order_by: [],
    };

    const columnDataTypes: TableColumn[] = [
      {
        name: 'idDeleted',
        dataType: 'bool',
        consoleDataType: 'boolean',
      },
    ];
    expect(adaptFormValuesToQuery(input, columnDataTypes)).toEqual(expected);
  });
});

it('does not try to parse non-integer values on integer field types', () => {
  const input: FiltersAndSortFormValues = {
    filters: [
      { column: 'text', operator: '$eq', value: 'aaaa' },
      { column: 'id', operator: '$eq', value: '1' },
    ],
    sorts: [],
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
    order_by: [],
  };

  const columnDataTypes: TableColumn[] = [
    {
      name: 'id',
      dataType: 'number',
      consoleDataType: 'number',
    },
    {
      name: 'text',
      dataType: 'string',
      consoleDataType: 'string',
    },
  ];
  expect(adaptFormValuesToQuery(input, columnDataTypes)).toEqual(expected);
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

describe('convertUserQueryToFiltersAndSortFormValues', () => {
  it('converts', () => {
    const userQuery: UserQuery = {
      where: {
        $and: [
          {
            id: {
              $gt: 1,
            },
          },
        ],
      },
      order_by: [
        {
          column: 'id',
          nulls: 'last',
          type: 'desc',
        },
      ],
    };
    const expected: FiltersAndSortFormValues = {
      filters: [
        {
          column: 'id',
          operator: '$gt',
          value: '1',
        },
      ],
      sorts: [
        {
          column: 'id',
          type: 'desc',
        },
      ],
    };

    expect(convertUserQueryToFiltersAndSortFormValues(userQuery)).toEqual(
      expected
    );
  });

  it('return nullish values as strings', () => {
    const userQuery: UserQuery = {
      where: {
        $and: [
          {
            id: {
              $gt: null,
            },
          },
        ],
      },
      order_by: [],
    };
    const expected: FiltersAndSortFormValues = {
      filters: [
        {
          column: 'id',
          operator: '$gt',
          value: 'null',
        },
      ],
      sorts: [],
    };

    expect(convertUserQueryToFiltersAndSortFormValues(userQuery)).toEqual(
      expected
    );
  });
});
