import { createFormData } from './createFormData';
import { createDefaultValues } from './createDefaultValues';
import { defaultValuesInput, formDataInput } from './mock';

const formDataMockResult: ReturnType<typeof createFormData> = {
  columns: ['ArtistId', 'Name'],
  roles: ['user'],
  supportedQueries: ['select'],
  tableNames: [['Album'], ['Artist']],
};

test('returns correctly formatted formData', () => {
  const result = createFormData(formDataInput);
  expect(result).toEqual(formDataMockResult);
});

const defaultValuesMockResult: ReturnType<typeof createDefaultValues> = {
  aggregationEnabled: true,
  checkType: 'none',
  allRowChecks: [],
  columns: {
    ArtistId: false,
    Name: true,
  },
  filter: {
    ArtistId: {
      _gt: 5,
    },
  },
  filterType: 'custom',
  operators: {
    filter: {
      columnOperator: '_gt',
      name: 'ArtistId',
      type: 'column',
      typeName: 'ArtistId',
    },
  },
  presets: [],
  rowCount: '3',
};

test('use default values returns values correctly', () => {
  const result = createDefaultValues(defaultValuesInput);
  expect(result).toEqual(defaultValuesMockResult);
});
