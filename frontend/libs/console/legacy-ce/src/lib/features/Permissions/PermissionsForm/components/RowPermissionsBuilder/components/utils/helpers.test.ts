import { PermissionType } from '../types';
import { updateKey } from './helpers';

describe('RowPermissionInput -> updateKey should', () => {
  test('create root value object', () => {
    const createRootValueInput = {
      permissionsState: {
        operators: {
          boolean: {
            label: 'Bool operators',
            items: [
              { name: '_and', value: '_and' },
              { name: '_not', value: '_not' },
              { name: '_or', value: '_or' },
            ],
          },
          exist: {
            label: 'Exist operators',
            items: [{ name: '_exists', value: '_exists' }],
          },
        },
        permissions: { _exists: { _table: {}, _where: {} } },
      },
      newKey: 'Series_reference',
      keyPath: ['_exists'],
      type: 'column' as PermissionType,
    };

    const result = updateKey(createRootValueInput);
    expect(result).toEqual({
      operators: {
        boolean: {
          items: [
            { name: '_and', value: '_and' },
            { name: '_not', value: '_not' },
            { name: '_or', value: '_or' },
          ],
          label: 'Bool operators',
        },
        exist: {
          items: [{ name: '_exists', value: '_exists' }],
          label: 'Exist operators',
        },
      },
      permissions: { Series_reference: { _eq: '' } },
    });
  });

  test('remove previous key value if the new is as operator', () => {
    const removePreviousInput = {
      permissionsState: {
        operators: {
          boolean: {
            label: 'Bool operators',
            items: [
              { name: '_and', value: '_and' },
              { name: '_not', value: '_not' },
              { name: '_or', value: '_or' },
            ],
          },
          exist: {
            label: 'Exist operators',
            items: [{ name: '_exists', value: '_exists' }],
          },
        },
        permissions: { Series_reference: { _eq: '' } },
      },
      newKey: '_neq',
      keyPath: ['Series_reference', '_eq'],
      type: 'comparator' as PermissionType,
    };

    const result = updateKey(removePreviousInput);
    expect(result).toEqual({
      operators: {
        boolean: {
          label: 'Bool operators',
          items: [
            { name: '_and', value: '_and' },
            { name: '_not', value: '_not' },
            { name: '_or', value: '_or' },
          ],
        },
        exist: {
          label: 'Exist operators',
          items: [{ name: '_exists', value: '_exists' }],
        },
      },
      permissions: { Series_reference: { _neq: '' } },
    });
  });

  test('strip object from array when removing permission', () => {
    const stripArrayLevelEmptyValuesInput = {
      permissionsState: {
        operators: {
          boolean: {
            label: 'Bool operators',
            items: [
              { name: '_and', value: '_and' },
              { name: '_not', value: '_not' },
              { name: '_or', value: '_or' },
            ],
          },
          exist: {
            label: 'Exist operators',
            items: [{ name: '_exists', value: '_exists' }],
          },
        },
        permissions: { _and: [{}, { Data_value: { _eq: '' } }] },
      },
      newKey: '',
      keyPath: ['_and', '1', 'Data_value'],
    };
    const result = updateKey(stripArrayLevelEmptyValuesInput);

    expect(result).toEqual({
      operators: {
        boolean: {
          label: 'Bool operators',
          items: [
            { name: '_and', value: '_and' },
            { name: '_not', value: '_not' },
            { name: '_or', value: '_or' },
          ],
        },
        exist: {
          label: 'Exist operators',
          items: [{ name: '_exists', value: '_exists' }],
        },
      },
      permissions: { _and: [{}] },
    });
  });

  test('reset root level state when picking empty value', () => {
    const resetRootLevelWhenPickingEmptyValue = {
      permissionsState: {
        operators: {
          boolean: {
            label: 'Bool operators',
            items: [
              { name: '_and', value: '_and' },
              { name: '_not', value: '_not' },
              { name: '_or', value: '_or' },
            ],
          },
          exist: {
            label: 'Exist operators',
            items: [{ name: '_exists', value: '_exists' }],
          },
        },
        permissions: {
          _not: { Series_reference: { _eq: 'X-Hasura-User-Id' } },
        },
      },
      newKey: '',
      keyPath: ['_not'],
    };

    const result = updateKey(resetRootLevelWhenPickingEmptyValue);

    expect(result).toEqual({
      operators: {
        boolean: {
          label: 'Bool operators',
          items: [
            { name: '_and', value: '_and' },
            { name: '_not', value: '_not' },
            { name: '_or', value: '_or' },
          ],
        },
        exist: {
          label: 'Exist operators',
          items: [{ name: '_exists', value: '_exists' }],
        },
      },
      permissions: {},
    });
  });

  test('reset nested level state when picking empty value', () => {
    const resetNestedLevelWhenPickingEmptyValue = {
      permissionsState: {
        operators: {
          boolean: {
            label: 'Bool operators',
            items: [
              { name: '_and', value: '_and' },
              { name: '_not', value: '_not' },
              { name: '_or', value: '_or' },
            ],
          },
          exist: {
            label: 'Exist operators',
            items: [{ name: '_exists', value: '_exists' }],
          },
        },
        permissions: {
          _not: {
            _and: [{}, { Series_reference: { _eq: 'X-Hasura-User-Id' } }],
          },
        },
      },
      newKey: '',
      keyPath: ['_not', '_and'],
    };

    const result = updateKey(resetNestedLevelWhenPickingEmptyValue);

    expect(result).toEqual({
      operators: {
        boolean: {
          label: 'Bool operators',
          items: [
            { name: '_and', value: '_and' },
            { name: '_not', value: '_not' },
            { name: '_or', value: '_or' },
          ],
        },
        exist: {
          label: 'Exist operators',
          items: [{ name: '_exists', value: '_exists' }],
        },
      },
      permissions: { _not: {} },
    });
  });

  test('switch from _not to _and', () => {
    const resetNestedLevelWhenPickingEmptyValue = {
      permissionsState: {
        operators: {
          boolean: {
            label: 'Bool operators',
            items: [
              { name: '_and', value: '_and' },
              { name: '_not', value: '_not' },
              { name: '_or', value: '_or' },
            ],
          },
          exist: {
            label: 'Exist operators',
            items: [{ name: '_exists', value: '_exists' }],
          },
        },
        permissions: { _not: {} },
      },
      newKey: '',
      keyPath: ['_not', '_and'],
    };

    const result = updateKey(resetNestedLevelWhenPickingEmptyValue);

    expect(result).toEqual({
      operators: {
        boolean: {
          label: 'Bool operators',
          items: [
            { name: '_and', value: '_and' },
            { name: '_not', value: '_not' },
            { name: '_or', value: '_or' },
          ],
        },
        exist: {
          label: 'Exist operators',
          items: [{ name: '_exists', value: '_exists' }],
        },
      },
      permissions: { _not: { _and: {} } },
    });
  });

  test('_is_null return boolean', () => {
    const testIsNullValueType = {
      permissionsState: {
        operators: {
          boolean: {
            label: 'Bool operators',
            items: [
              { name: '_and', value: '_and' },
              { name: '_not', value: '_not' },
              { name: '_or', value: '_or' },
            ],
          },
          exist: {
            label: 'Exist operators',
            items: [{ name: '_exists', value: '_exists' }],
          },
        },
        permissions: { id: { _eq: '' } },
      },
      newKey: '_is_null',
      keyPath: ['id', '_eq'],
    };

    const result = updateKey(testIsNullValueType);

    expect(result).toEqual({
      operators: {
        boolean: {
          label: 'Bool operators',
          items: [
            { name: '_and', value: '_and' },
            { name: '_not', value: '_not' },
            { name: '_or', value: '_or' },
          ],
        },
        exist: {
          label: 'Exist operators',
          items: [{ name: '_exists', value: '_exists' }],
        },
      },
      permissions: { id: { _is_null: false } },
    });
  });
});
