import { act } from 'react-dom/test-utils';
import { renderHook } from '@testing-library/react-hooks';
import { useTableMachine } from '../useTableMachine';

describe('table state machine test', () => {
  // close transitions to formOpen
  test('transitions from closed to formOpen', async () => {
    const { result } = renderHook(() => useTableMachine());

    const { 1: send } = result.current;

    act(() => {
      send({
        type: 'FORM_OPEN',
        selectedForm: {
          roleName: 'user',
          queryType: 'insert',
          accessType: 'fullAccess',
        },
      });
    });

    const [state] = result.current;

    expect(state.value).toEqual('formOpen');
    expect(state.context.selectedForm.roleName).toEqual('user');
    expect(state.context.selectedForm.queryType).toEqual('insert');
    expect(state.context.selectedForm.accessType).toEqual('fullAccess');
  });

  // closed to bulk open
  test('transitions from closed to bulkOpen', async () => {
    const { result } = renderHook(() => useTableMachine());

    const { 1: send } = result.current;

    act(() => {
      send({
        type: 'BULK_OPEN',
        roleName: 'user',
      });
    });

    act(() => {
      send({
        type: 'BULK_OPEN',
        roleName: 'user2',
      });
    });

    const [state] = result.current;

    expect(state.value).toEqual('bulkOpen');
    expect(state.context.selectedForm).toEqual({});
    expect(state.context.bulkSelections).toEqual(['user', 'user2']);
  });

  // form open to closed
  test('transitions from formOpen to closed', async () => {
    const { result } = renderHook(() => useTableMachine());

    const { 1: send } = result.current;

    act(() => {
      send({
        type: 'FORM_OPEN',
        selectedForm: {
          roleName: 'user',
          queryType: 'insert',
          accessType: 'fullAccess',
        },
      });
    });

    act(() => {
      send('CLOSE');
    });

    const [state] = result.current;

    expect(state.value).toEqual('closed');
  });

  // bulk to closed
  test('transitions from bulkOpen to closed', async () => {
    const { result } = renderHook(() => useTableMachine());

    const { 1: send } = result.current;

    act(() => {
      send({
        type: 'BULK_OPEN',
        roleName: 'user',
      });
    });

    act(() => {
      send('CLOSE');
    });

    const [state] = result.current;

    expect(state.value).toEqual('closed');
  });

  // bulk to formOpen
  test('transitions from bulkOpen to formOpen', async () => {
    const { result } = renderHook(() => useTableMachine());

    const { 1: send } = result.current;

    act(() => {
      send({
        type: 'BULK_OPEN',
        roleName: 'user',
      });
    });

    act(() => {
      send({
        type: 'FORM_OPEN',
        selectedForm: {
          roleName: 'user',
          queryType: 'insert',
          accessType: 'fullAccess',
        },
      });
    });

    const [state] = result.current;

    expect(state.value).toEqual('formOpen');
  });

  // form open to bulk
  test('transitions from formOpen to bulkOpen', async () => {
    const { result } = renderHook(() => useTableMachine());

    const { 1: send } = result.current;

    act(() => {
      send({
        type: 'FORM_OPEN',
        selectedForm: {
          roleName: 'user',
          queryType: 'insert',
          accessType: 'fullAccess',
        },
      });
    });

    act(() => {
      send({
        type: 'BULK_OPEN',
        roleName: 'user',
      });
    });

    const [state] = result.current;

    expect(state.value).toEqual('bulkOpen');
  });

  // form open to different form open
  test('transitions from formOpen to different formOpen', async () => {
    const { result } = renderHook(() => useTableMachine());

    const { 1: send } = result.current;

    act(() => {
      send({
        type: 'FORM_OPEN',
        selectedForm: {
          roleName: 'user',
          queryType: 'insert',
          accessType: 'fullAccess',
        },
      });
    });

    act(() => {
      send({
        type: 'FORM_OPEN',
        selectedForm: {
          roleName: 'user',
          queryType: 'update',
          accessType: 'partialAccess',
        },
      });
    });

    const [state] = result.current;

    expect(state.value).toEqual('formOpen');
    expect(state.context.selectedForm.roleName).toEqual('user');
    expect(state.context.selectedForm.queryType).toEqual('update');
    expect(state.context.selectedForm.accessType).toEqual('partialAccess');
  });

  // new role to focus input
  test('transitions from closed to updateRoleName', async () => {
    const { result } = renderHook(() => useTableMachine());

    const { 1: send } = result.current;

    act(() => {
      send({
        type: 'FORM_OPEN',
        selectedForm: {
          roleName: '',
          queryType: 'insert',
          accessType: 'fullAccess',
          isNewRole: true,
        },
      });
    });

    const [state] = result.current;

    expect(state.value).toEqual('updateRoleName');
  });

  // new role opens if newRoleName input
  test('transitions to formOpen after adding new role name', async () => {
    const { result } = renderHook(() => useTableMachine());

    const { 1: send } = result.current;

    // try to open form
    act(() => {
      send({
        type: 'FORM_OPEN',
        selectedForm: {
          roleName: '',
          queryType: 'insert',
          accessType: 'fullAccess',
          isNewRole: true,
        },
      });
    });

    // type a new name
    act(() => {
      send({
        type: 'NEW_ROLE_NAME',
        newRoleName: 'user2',
      });
    });

    // open form with input
    act(() => {
      send({
        type: 'FORM_OPEN',
        selectedForm: {
          roleName: 'user2',
          queryType: 'update',
          accessType: 'noAccess',
          isNewRole: true,
        },
      });
    });

    const [state] = result.current;

    expect(state.value).toEqual('formOpen');
    expect(state.context.selectedForm.roleName).toEqual('user2');
    expect(state.context.selectedForm.queryType).toEqual('update');
    expect(state.context.selectedForm.accessType).toEqual('noAccess');
  });

  // new role close and clear
  test('transitions to formOpen after adding new role name', async () => {
    const { result } = renderHook(() => useTableMachine());

    const { 1: send } = result.current;

    // try to open form
    act(() => {
      send({
        type: 'FORM_OPEN',
        selectedForm: {
          roleName: '',
          queryType: 'insert',
          accessType: 'fullAccess',
          isNewRole: true,
        },
      });
    });

    // type a new name
    act(() => {
      send({
        type: 'NEW_ROLE_NAME',
        newRoleName: 'user2',
      });
    });

    // open form with input
    act(() => {
      send({
        type: 'FORM_OPEN',
        selectedForm: {
          roleName: 'user2',
          queryType: 'update',
          accessType: 'noAccess',
          isNewRole: true,
        },
      });
    });

    act(() => {
      send('CLOSE');
    });

    const [state] = result.current;

    expect(state.value).toEqual('closed');
    expect(state.context.newRoleName).toEqual('');
  });
});
