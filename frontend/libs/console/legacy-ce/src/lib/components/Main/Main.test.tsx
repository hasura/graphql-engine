import { beforeEach, describe, expect, it, vi } from 'vitest';
import React, { Profiler } from 'react';
import { render } from '@testing-library/react';
import { METADATA_STATUS_PATH } from '@hasura/shared/types';

const navigate = vi.fn();
vi.mock('react-router', () => ({
  __esModule: true,
  useNavigate: () => navigate,
  useLocation: () => ({ pathname: '/' }),
  Outlet: () => null,
}));

const refreshCatalogState = vi.fn();
vi.mock('../../telemetry/useCatalogState', () => ({
  __esModule: true,
  default: () => ({ refreshCatalogState }),
}));

vi.mock('@hasura/shared/context', () => ({
  __esModule: true,
  useAppContext: () => ({ serverVersion: 'v2.0.0' }),
}));

vi.mock('../../telemetry/store', () => ({
  __esModule: true,
  useTelemetryStore: () => ({ consoleState: null }),
}));

let reloadResult: {
  reloadMetadata: () => void;
  inconsistentMetadata: any;
  isLoading: boolean;
};
vi.mock('@hasura/metadata/api', () => ({
  __esModule: true,
  useMetadata: () => ({ data: { metadata: {} } }),
  useReloadMetadata: () => reloadResult,
}));

// Child components are irrelevant to the effects under test.
vi.mock('../Common/Onboarding', () => ({
  __esModule: true,
  default: () => null,
}));
vi.mock('./components/UpdateVersion', () => ({
  __esModule: true,
  UpdateVersion: () => null,
}));
vi.mock('./components/MainHeader', () => ({
  __esModule: true,
  default: () => null,
}));
vi.mock('../Error/ErrorBoundary', () => ({
  __esModule: true,
  default: ({ children }: { children: React.ReactNode }) => <>{children}</>,
}));

import Main from './Main';

beforeEach(() => {
  vi.clearAllMocks();
  reloadResult = {
    reloadMetadata: vi.fn(),
    inconsistentMetadata: undefined,
    isLoading: true,
  };
});

describe('Main effects', () => {
  it('refreshes catalog state exactly once on mount', () => {
    render(<Main />);
    expect(refreshCatalogState).toHaveBeenCalledTimes(1);
  });

  it('does not refresh catalog state again on re-render', () => {
    const { rerender } = render(<Main />);
    rerender(<Main />);
    rerender(<Main />);
    expect(refreshCatalogState).toHaveBeenCalledTimes(1);
  });

  it('does not navigate while metadata is still loading', () => {
    reloadResult = {
      reloadMetadata: vi.fn(),
      inconsistentMetadata: undefined,
      isLoading: true,
    };
    render(<Main />);
    expect(navigate).not.toHaveBeenCalled();
  });

  it('does not navigate when metadata is consistent', () => {
    reloadResult = {
      reloadMetadata: vi.fn(),
      inconsistentMetadata: { is_consistent: true },
      isLoading: false,
    };
    render(<Main />);
    expect(navigate).not.toHaveBeenCalled();
  });

  it('navigates to the metadata status page when metadata becomes inconsistent', () => {
    const { rerender } = render(<Main />);
    expect(navigate).not.toHaveBeenCalled();

    reloadResult = {
      reloadMetadata: vi.fn(),
      inconsistentMetadata: { is_consistent: false },
      isLoading: false,
    };
    rerender(<Main />);

    expect(navigate).toHaveBeenCalledTimes(1);
    expect(navigate).toHaveBeenCalledWith(METADATA_STATUS_PATH);
  });

  it('settles without a runaway render loop', () => {
    let commitCount = 0;
    render(
      <Profiler id="main" onRender={() => (commitCount += 1)}>
        <Main />
      </Profiler>,
    );
    expect(commitCount).toBeLessThanOrEqual(2);
  });
});
