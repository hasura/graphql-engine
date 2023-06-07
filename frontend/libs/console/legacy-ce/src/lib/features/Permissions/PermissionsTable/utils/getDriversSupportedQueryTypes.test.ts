import { getDriversSupportedQueryTypes } from './getDriversSupportedQueryTypes';
import { capabilitiesResponse } from '../../PermissionsForm/mocks/dataStubs';

test('getDriversSupportedQueryTypes should return support for all query types', () => {
  const { capabilities } = capabilitiesResponse;
  const result = getDriversSupportedQueryTypes(capabilities as any);
  expect(result).toEqual(['delete', 'insert', 'update', 'select']);
});

test('getDriversSupportedQueryTypes should return support queries (BigQuery)', () => {
  const result = getDriversSupportedQueryTypes({ queries: {} } as any);
  expect(result).toEqual(['select']);
});
