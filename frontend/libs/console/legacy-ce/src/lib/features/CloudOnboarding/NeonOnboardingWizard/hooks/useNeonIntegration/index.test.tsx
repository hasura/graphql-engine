import { renderHook } from '@testing-library/react';
import { useNeonIntegration } from './index';

// Controllable statuses for the composed hooks.
let neonOauthStatus: any = { status: 'idle' };
let neonDBState: any = { status: 'idle' };
let hasuraState: any = { status: 'idle' };

const startNeonOAuth = vi.fn();
const createNeonDatabase = vi.fn();
const resetNeonDBCreationState = vi.fn();
const addHasuraDatasource = vi.fn();

vi.mock('../useNeonOAuth', () => ({
  useNeonOAuth: () => ({ startNeonOAuth, neonOauthStatus }),
}));
vi.mock('../useNeonDatabase', () => ({
  useNeonDatabase: () => ({
    create: createNeonDatabase,
    state: neonDBState,
    reset: resetNeonDBCreationState,
  }),
}));
vi.mock('./useCreateHasuraCloudDatasource', () => ({
  useCreateHasuraCloudDatasource: () => ({
    state: hasuraState,
    addHasuraDatasource,
  }),
}));

describe('useNeonIntegration', () => {
  beforeEach(() => {
    neonOauthStatus = { status: 'idle' };
    neonDBState = { status: 'idle' };
    hasuraState = { status: 'idle' };
    vi.clearAllMocks();
  });

  it('starts adding the datasource once neon db creation succeeds and hasura is idle', () => {
    neonDBState = {
      status: 'success',
      payload: { databaseUrl: 'postgres://db', email: 'a@b.com' },
    };
    hasuraState = { status: 'idle' };

    renderHook(() =>
      useNeonIntegration('ds', vi.fn(), vi.fn(), 'data-manage-create'),
    );

    expect(addHasuraDatasource).toHaveBeenCalledTimes(1);
  });

  it('invokes the success callback once when the whole flow succeeds', () => {
    neonDBState = {
      status: 'success',
      payload: { databaseUrl: 'postgres://db', email: 'a@b.com' },
    };
    hasuraState = {
      status: 'success',
      payload: { dataSourceName: 'ds', envVar: 'ENV' },
    };
    const onSuccess = vi.fn();

    renderHook(() =>
      useNeonIntegration('ds', onSuccess, vi.fn(), 'data-manage-create'),
    );

    expect(onSuccess).toHaveBeenCalledTimes(1);
    expect(onSuccess).toHaveBeenCalledWith('ds');
  });

  it('does NOT re-fire the success callback when the parent passes a new callback on re-render', () => {
    neonDBState = {
      status: 'success',
      payload: { databaseUrl: 'postgres://db', email: 'a@b.com' },
    };
    hasuraState = {
      status: 'success',
      payload: { dataSourceName: 'ds', envVar: 'ENV' },
    };

    const firstCb = vi.fn();
    const secondCb = vi.fn();

    const { rerender } = renderHook(
      ({ cb }: { cb: (name: string) => void }) =>
        useNeonIntegration('ds', cb, vi.fn(), 'data-manage-create'),
      { initialProps: { cb: firstCb } },
    );

    expect(firstCb).toHaveBeenCalledTimes(1);

    // Parent re-renders with a brand new inline callback but the statuses are
    // unchanged: the effect must not run again.
    rerender({ cb: secondCb });

    expect(firstCb).toHaveBeenCalledTimes(1);
    expect(secondCb).not.toHaveBeenCalled();
  });
});
