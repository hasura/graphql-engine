import { copyQueryTypePermissions } from './copyQueryTypePermissions';

describe('copyQueryTypePermissions should', () => {
  test('handle pre_update mapping', () => {
    const result = copyQueryTypePermissions('insert', 'update', 'pre_update', {
      check: { Name: { _eq: 'select' } },
      columns: {},
    });
    expect(result).toEqual(['filter', { Name: { _eq: 'select' } }]);
  });

  test('handle pre_update mapping', () => {
    const result = copyQueryTypePermissions('insert', 'update', 'post_update', {
      check: { Name: { _eq: 'insert' } },
      columns: {},
    });
    expect(result).toEqual(['check', { Name: { _eq: 'insert' } }]);
  });
});
