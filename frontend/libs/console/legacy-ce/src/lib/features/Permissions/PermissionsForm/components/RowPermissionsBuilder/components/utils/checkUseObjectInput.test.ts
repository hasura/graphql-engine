import { checkUseObjectInput } from '../ValueInputType';

describe('RowPermissionInput -> updateKey should', () => {
  test('create root value object', () => {
    const result = checkUseObjectInput('_eq', {
      name: '_eq',
      inputStructure: 'object',
      type: 'comparision',
    });
    expect(result).toEqual(false);
  });
  test('create root value object', () => {
    const result = checkUseObjectInput('json', {
      name: '_contains',
      inputStructure: 'object',
      type: 'jsonb',
    });
    expect(result).toEqual(true);
  });
});
