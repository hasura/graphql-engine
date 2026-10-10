import { act, renderHook } from '@testing-library/react';
import useRemoteSchemaHeaderForm from './useRemoteSchemaHeaderForm';

const emptyHeader = { name: '', type: '', value: '' };

describe('useRemoteSchemaHeaderForm', () => {
  it('starts with a single empty header row', () => {
    const { result } = renderHook(() => useRemoteSchemaHeaderForm());

    expect(result.current.headers).toEqual([emptyHeader]);
  });

  it('appends a new empty header row', () => {
    const { result } = renderHook(() => useRemoteSchemaHeaderForm());

    act(() => result.current.addNewHeader());

    expect(result.current.headers).toEqual([emptyHeader, emptyHeader]);
  });

  it('edits only the targeted row when changing key, value and type', () => {
    const { result } = renderHook(() => useRemoteSchemaHeaderForm());

    act(() => result.current.addNewHeader());

    act(() => result.current.changeHeaderKey('x-hasura-role', 1));
    act(() => result.current.changeHeaderValue('admin', 1));
    act(() => result.current.changeHeaderType('value', 1));

    expect(result.current.headers).toEqual([
      emptyHeader,
      { name: 'x-hasura-role', value: 'admin', type: 'value' },
    ]);
  });

  it('removes the header at the given index', () => {
    const { result } = renderHook(() => useRemoteSchemaHeaderForm());

    act(() => result.current.addNewHeader());
    act(() => result.current.changeHeaderKey('keep', 0));
    act(() => result.current.changeHeaderKey('drop', 1));

    act(() => result.current.removeHeader(1));

    expect(result.current.headers).toEqual([{ ...emptyHeader, name: 'keep' }]);
  });
});
