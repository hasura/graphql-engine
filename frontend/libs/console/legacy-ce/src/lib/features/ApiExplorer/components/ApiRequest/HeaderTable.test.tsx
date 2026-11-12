import React from 'react';
import { act, render } from '@testing-library/react';
import { vi } from 'vitest';
import { AuthContext } from '@hasura/shared/context';
import type { AuthService } from '@hasura/shared/context';
import { testWrapper } from '@hasura/shared/testing';
import type { DataHeader } from '@hasura/shared/types';
import HeaderTable from './HeaderTable';
import { applyAuthHeaders } from './utils';

const NOW = new Date('2026-01-01T00:00:00Z').getTime();

const headers: DataHeader[] = [
  {
    key: 'hasura-collaborator-token',
    value: 'IDToken old',
    selected: true,
    isDisabled: true,
  },
  { key: '', value: '', selected: true, isDisabled: false },
];

const renderTable = (auth: Partial<AuthService<any>>) => {
  const updateAuthHeaders = vi.fn();
  const authService = {
    isAuthenticated: true,
    authType: 'none',
    getHeaders: async () => ({}),
    authenticate: async () => true,
    initialize: async () => undefined,
    logout: () => undefined,
    ...auth,
  } as AuthService<any>;

  const utils = render(
    <AuthContext.Provider value={authService}>
      <HeaderTable
        headers={headers}
        serverConfig={undefined}
        handleHeaderFocus={vi.fn()}
        handleHeaderUnfocus={vi.fn()}
        removeRequestHeader={vi.fn()}
        changeRequestHeader={vi.fn()}
        updateAuthHeaders={updateAuthHeaders}
        analyzeBearerToken={vi.fn()}
        analyzingToken={{ isAnalyzing: false, headerRow: -1 }}
      />
    </AuthContext.Provider>,
    { wrapper: testWrapper as React.FC<{ children: React.ReactNode }> },
  );

  return { ...utils, updateAuthHeaders, authService };
};

describe('HeaderTable auth header refresh', () => {
  beforeEach(() => {
    vi.useFakeTimers({ toFake: ['setTimeout', 'clearTimeout', 'Date'] });
    vi.setSystemTime(NOW);
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  it('does nothing when the session does not expire', async () => {
    const getHeaders = vi.fn(async () => ({}));
    renderTable({ getHeaders });

    await act(() => vi.advanceTimersByTimeAsync(60 * 60 * 1000));

    expect(getHeaders).not.toHaveBeenCalled();
  });

  it('reloads the auth headers shortly before the session expires', async () => {
    const getHeaders = vi.fn(async () => ({
      'hasura-collaborator-token': 'IDToken new',
    }));
    const { updateAuthHeaders } = renderTable({
      getHeaders,
      sessionExpiry: NOW + 5 * 60 * 1000,
    });

    // synced once on mount
    await act(() => vi.advanceTimersByTimeAsync(0));
    expect(getHeaders).toHaveBeenCalledTimes(1);

    await act(() => vi.advanceTimersByTimeAsync(4 * 60 * 1000));
    expect(getHeaders).toHaveBeenCalledTimes(1);

    // 30s before the expiry
    await act(() => vi.advanceTimersByTimeAsync(30 * 1000));
    expect(getHeaders).toHaveBeenCalledTimes(2);
    expect(updateAuthHeaders).toHaveBeenLastCalledWith({
      'hasura-collaborator-token': 'IDToken new',
    });
  });

  it('reschedules when the session is refreshed', async () => {
    const getHeaders = vi.fn(async () => ({}));
    const { rerender, updateAuthHeaders } = renderTable({
      getHeaders,
      sessionExpiry: NOW + 5 * 60 * 1000,
    });
    await act(() => vi.advanceTimersByTimeAsync(0));

    const newGetHeaders = vi.fn(async () => ({
      'hasura-collaborator-token': 'IDToken refreshed',
    }));
    rerender(
      <AuthContext.Provider
        value={
          {
            isAuthenticated: true,
            authType: 'none',
            getHeaders: newGetHeaders,
            sessionExpiry: NOW + 60 * 60 * 1000,
            authenticate: async () => true,
            initialize: async () => undefined,
            logout: () => undefined,
          } as AuthService<any>
        }
      >
        <HeaderTable
          headers={headers}
          serverConfig={undefined}
          handleHeaderFocus={vi.fn()}
          handleHeaderUnfocus={vi.fn()}
          removeRequestHeader={vi.fn()}
          changeRequestHeader={vi.fn()}
          updateAuthHeaders={updateAuthHeaders}
          analyzeBearerToken={vi.fn()}
          analyzingToken={{ isAnalyzing: false, headerRow: -1 }}
        />
      </AuthContext.Provider>,
    );

    // the new session is synced into the rows right away
    await act(() => vi.advanceTimersByTimeAsync(0));
    expect(newGetHeaders).toHaveBeenCalledTimes(1);
    expect(updateAuthHeaders).toHaveBeenLastCalledWith({
      'hasura-collaborator-token': 'IDToken refreshed',
    });

    // the timer of the old session was cleared
    await act(() => vi.advanceTimersByTimeAsync(10 * 60 * 1000));
    expect(getHeaders).toHaveBeenCalledTimes(1);
    expect(newGetHeaders).toHaveBeenCalledTimes(1);
  });

  it('does not reload in a tight loop when the session expiry is in the past', async () => {
    const getHeaders = vi.fn(async () => ({}));
    renderTable({ getHeaders, sessionExpiry: NOW - 1000 });

    await act(() => vi.advanceTimersByTimeAsync(4 * 1000));
    expect(getHeaders).toHaveBeenCalledTimes(1);

    await act(() => vi.advanceTimersByTimeAsync(60 * 1000));
    expect(getHeaders).toHaveBeenCalledTimes(2);
  });
});

describe('applyAuthHeaders', () => {
  it('replaces the values of matching headers case-insensitively', () => {
    const row = { selected: true, isDisabled: false };
    const result = applyAuthHeaders(
      [
        { ...row, key: 'Hasura-SSO-Token', value: 'IDToken old' },
        { ...row, key: 'x-hasura-role', value: 'user' },
      ],
      { 'hasura-sso-token': 'IDToken new' },
    );

    expect(result).toEqual([
      { ...row, key: 'Hasura-SSO-Token', value: 'IDToken new' },
      { ...row, key: 'x-hasura-role', value: 'user' },
    ]);
  });

  it('returns null when nothing changed', () => {
    expect(
      applyAuthHeaders(headers, {
        'hasura-collaborator-token': 'IDToken old',
      }),
    ).toBeNull();
    expect(applyAuthHeaders(headers, {})).toBeNull();
  });

  it('does not add headers the user removed', () => {
    expect(
      applyAuthHeaders(headers, { 'hasura-sso-token': 'IDToken new' }),
    ).toBeNull();
  });
});
