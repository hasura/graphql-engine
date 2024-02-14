import { getPermissionTypes } from './typeProviderHelpers';
import { tables } from './__tests__/fixtures/tables';

test('getPermissionTypes', () => {
  const table = ['Album'];
  const permissions = {
    Artist: {
      Albums: {},
    },
  };
  expect(getPermissionTypes(tables, table, permissions)).toEqual({
    Artist: {
      type: 'relationship',
      Albums: {
        type: 'relationship',
      },
    },
  });
});
