import { act, renderHook } from '@testing-library/react';
import useRemoteSchemaRelationshipForm from './useRemoteSchemaRelationshipForm';

const defaultPermissionEdit = {
  newRole: '',
  isNewRole: false,
  isNewPerm: false,
  role: '',
};

describe('useRemoteSchemaRelationshipForm', () => {
  it('starts with empty selection and edit state', () => {
    const { result } = renderHook(() => useRemoteSchemaRelationshipForm());

    expect(result.current.bulkSelect).toEqual([]);
    expect(result.current.isEditing).toBe(false);
    expect(result.current.schemaDefinition).toBe('');
    expect(result.current.permissionEdit).toEqual(defaultPermissionEdit);
  });

  it('adds and removes roles from the bulk selection', () => {
    const { result } = renderHook(() => useRemoteSchemaRelationshipForm());

    act(() => result.current.permSetBulkSelect(true, 'admin'));
    act(() => result.current.permSetBulkSelect(true, 'user'));
    expect(result.current.bulkSelect).toEqual(['admin', 'user']);

    act(() => result.current.permSetBulkSelect(false, 'admin'));
    expect(result.current.bulkSelect).toEqual(['user']);
  });

  it('sets the role name without touching other edit fields', () => {
    const { result } = renderHook(() => useRemoteSchemaRelationshipForm());

    act(() => result.current.permSetRoleName('editor'));

    expect(result.current.permissionEdit).toEqual({
      ...defaultPermissionEdit,
      role: 'editor',
    });
  });

  it('opens the editor with role/new flags and an empty filter', () => {
    const { result } = renderHook(() => useRemoteSchemaRelationshipForm());

    act(() => result.current.permOpenEdit('manager', true, false));

    expect(result.current.isEditing).toBe(true);
    expect(result.current.permissionEdit).toEqual({
      ...defaultPermissionEdit,
      role: 'manager',
      isNewRole: true,
      isNewPerm: false,
      filter: {},
    });
  });

  it('resets editing state when the editor is closed', () => {
    const { result } = renderHook(() => useRemoteSchemaRelationshipForm());

    act(() => result.current.permOpenEdit('manager', true, true));
    act(() => result.current.permCloseEdit());

    expect(result.current.isEditing).toBe(false);
    expect(result.current.permissionEdit).toEqual(defaultPermissionEdit);
  });

  it('tracks the schema definition text', () => {
    const { result } = renderHook(() => useRemoteSchemaRelationshipForm());

    act(() =>
      result.current.setSchemaDefinition('type Query { hello: String }'),
    );

    expect(result.current.schemaDefinition).toBe(
      'type Query { hello: String }',
    );
  });
});
