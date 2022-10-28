import { createDefaultValues } from './useDefaultValues';
import { input } from './mock';

const mockResult: ReturnType<typeof createDefaultValues> = {
  aggregationEnabled: true,
  allRowChecks: [],
  backendOnly: false,
  check: {},
  checkType: 'none',
  clonePermissions: [],
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
  const result = createDefaultValues(input);
  expect(result).toEqual(mockResult);
});
