import { act, renderHook } from '@testing-library/react';
import type { RemoteSchemaCustomization } from '@hasura/shared/types';
import useRemoteSchemaForm from './useRemoteSchemaForm';

describe('useRemoteSchemaForm', () => {
  it('starts with the create-form defaults', () => {
    const { result } = renderHook(() => useRemoteSchemaForm());

    expect(result.current.formState).toEqual({
      name: '',
      manualUrl: null,
      envName: null,
      timeoutConf: '60',
      forwardClientHeaders: false,
      comment: '',
      customization: undefined,
    });
  });

  it('updates the plain text fields independently', () => {
    const { result } = renderHook(() => useRemoteSchemaForm());

    act(() => result.current.setName('my-remote-schema'));
    act(() => result.current.setComment('a description'));
    act(() => result.current.setTimeoutConf('120'));

    expect(result.current.formState.name).toBe('my-remote-schema');
    expect(result.current.formState.comment).toBe('a description');
    expect(result.current.formState.timeoutConf).toBe('120');
  });

  it('toggles forwardClientHeaders on and off', () => {
    const { result } = renderHook(() => useRemoteSchemaForm());

    expect(result.current.formState.forwardClientHeaders).toBe(false);

    act(() => result.current.toggleForwardClientHeaders());
    expect(result.current.formState.forwardClientHeaders).toBe(true);

    act(() => result.current.toggleForwardClientHeaders());
    expect(result.current.formState.forwardClientHeaders).toBe(false);
  });

  it('stores the provided customization object', () => {
    const { result } = renderHook(() => useRemoteSchemaForm());
    const customization: RemoteSchemaCustomization = {
      root_fields_namespace: 'namespace_',
    };

    act(() => result.current.setCustomization(customization));

    expect(result.current.formState.customization).toEqual(customization);
  });

  it('clears the manual URL when an env URL is chosen', () => {
    const { result } = renderHook(() => useRemoteSchemaForm());

    act(() => result.current.setManualURL('https://example.com/graphql'));
    expect(result.current.formState.manualUrl).toBe(
      'https://example.com/graphql',
    );
    expect(result.current.formState.envName).toBe('');

    act(() => result.current.setEnvURL('REMOTE_SCHEMA_URL'));
    expect(result.current.formState.envName).toBe('REMOTE_SCHEMA_URL');
    expect(result.current.formState.manualUrl).toBe('');
  });

  it('clears the env URL when a manual URL is chosen', () => {
    const { result } = renderHook(() => useRemoteSchemaForm());

    act(() => result.current.setEnvURL('REMOTE_SCHEMA_URL'));
    expect(result.current.formState.envName).toBe('REMOTE_SCHEMA_URL');
    expect(result.current.formState.manualUrl).toBe('');

    act(() => result.current.setManualURL('https://example.com/graphql'));
    expect(result.current.formState.manualUrl).toBe(
      'https://example.com/graphql',
    );
    expect(result.current.formState.envName).toBe('');
  });
});
