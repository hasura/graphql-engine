import { createFormData } from './createFormData';
import { createDefaultValues } from './createDefaultValues';
import { useFormDataCreateDefaultValuesMock, createFormDataMock } from './mock';

const formDataMockResult = {
  roles: ['asdf', 'new', 'sdfsf', 'testrole', 'user'],
  supportedQueries: ['select'],
  tableNames: [{ dataset: 'bigquery_sample', name: 'sample_table' }],
  columns: [
    'Series_reference',
    'Period',
    'Data_value',
    'Suppressed',
    'STATUS',
    'UNITS',
    'Magnitude',
    'Subject',
    'Group',
    'Series_title_1',
    'Series_title_2',
    'Series_title_3',
    'Series_title_4',
    'Series_title_5',
  ],
};

test('returns correctly formatted formData', () => {
  const result = createFormData(createFormDataMock);
  expect(result).toEqual(formDataMockResult);
});

const defaultValuesMockResult: ReturnType<typeof createDefaultValues> = {
  queryType: 'select',
  filterType: 'custom',
  columns: {
    Series_reference: false,
    Period: false,
    Data_value: false,
    Suppressed: false,
    STATUS: false,
    UNITS: false,
    Magnitude: false,
    Subject: false,
    Group: false,
    Series_title_1: false,
    Series_title_2: false,
    Series_title_3: false,
    Series_title_4: false,
    Series_title_5: false,
  },
  supportedOperators: [
    { name: 'equals', value: '_eq' },
    { name: 'not equals', value: '_neq' },
    { name: 'in', value: '_in', defaultValue: '[]' },
    { name: 'nin', value: '_nin', defaultValue: '[]' },
    { name: '>', value: '_gt' },
    { name: '<', value: '_lt' },
    { name: '>=', value: '_gte' },
    { name: '<=', value: '_lte' },
    { name: 'like', value: '_like', defaultValue: '%%' },
    { name: 'not like', value: '_nlike', defaultValue: '%%' },
  ],
  comment: '',
  filter: { _not: { Data_value: { _eq: 1337 } } },
  rowCount: '0',
  aggregationEnabled: false,
  operators: {
    filter: {
      name: '_not',
      typeName: '_not',
      type: 'boolOperator',
      _not: {
        name: 'Data_value',
        typeName: 'Data_value',
        type: 'column',
        columnOperator: '_eq',
      },
    },
  },
  query_root_fields: null,
  subscription_root_fields: null,
};

test('use default values returns values correctly', () => {
  const result = createDefaultValues(useFormDataCreateDefaultValuesMock);

  expect(result).toEqual(defaultValuesMockResult);
});
