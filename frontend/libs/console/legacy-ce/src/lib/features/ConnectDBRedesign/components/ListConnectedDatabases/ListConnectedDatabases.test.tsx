import { render, screen } from '@testing-library/react';

// ---------------------------------------------------------------------------
// ListConnectedDatabases pulls in a large graph of data hooks. Mock them so the
// test can focus on one behavior: the "Accelerate project" section is now
// DERIVED from the latency-check result during render (previously mirrored into
// local state inside an effect). We drive the latency hook's return value and
// assert the rendered output + that the error path fires its toast exactly once
// (no cascading re-render loop).
// ---------------------------------------------------------------------------

const { hasuraToastMock } = vi.hoisted(() => ({ hasuraToastMock: vi.fn() }));

let latencyState: any;
vi.mock('../../hooks/useDatabaseLatencyCheck', () => ({
  useDatabaseLatencyCheck: () => latencyState,
}));
vi.mock('../../hooks/useReloadSource', () => ({
  useReloadSource: () => ({ mutate: vi.fn(), isPending: false }),
}));
vi.mock('../../hooks/useUpdateProjectRegion', () => ({
  useUpdateProjectRegion: () => ({ mutate: vi.fn() }),
}));

vi.mock('@hasura/metadata/api', () => ({
  useMetadata: () => ({
    data: [],
    isLoading: false,
    isFetching: false,
    refetch: vi.fn(),
  }),
  useInconsistentMetadata: () => ({ data: [], isLoading: false }),
  inconsistentSourcesSelector: vi.fn(),
  useDropSource: () => ({ dropSource: vi.fn(), isPending: false }),
}));

vi.mock('@hasura/metadata/data-source', () => ({
  useDatabaseVersion: () => ({ data: [], isLoading: false, error: undefined }),
  NotImplementedError: class extends Error {},
}));

vi.mock('@hasura/shared/context', () => ({
  useAppContext: () => ({ envVars: {} }),
}));

vi.mock('@hasura/shared/utils', async (importActual) => {
  const actual = await importActual<any>();
  return {
    ...actual,
    isCloudConsole: () => true,
    getProjectId: () => 'project-id',
  };
});

vi.mock('@hasura/shared/ui', async (importActual) => {
  const actual = await importActual<any>();
  return {
    ...actual,
    hasuraToast: (...args: unknown[]) => hasuraToastMock(...args),
    useDestructiveAlert: () => ({ destructivePrompt: vi.fn() }),
  };
});

vi.mock('./parts', () => ({
  AccelerateProject: () => <div data-testid="accelerate-section" />,
  Details: () => null,
  DisplayDetails: () => null,
  InconsistentSourceDetails: () => null,
  LatencyBadge: () => null,
}));

vi.mock('react-router', () => ({ useNavigate: () => vi.fn() }));

import { ListConnectedDatabases } from './ListConnectedDatabases';

const baseLatency = {
  refetch: vi.fn(),
  isLoading: false,
};

describe('ListConnectedDatabases - Accelerate project section (derived)', () => {
  beforeEach(() => {
    hasuraToastMock.mockReset();
  });

  it('hides the section and offers "Check latency" before a check runs', () => {
    latencyState = {
      ...baseLatency,
      data: {},
      isSuccess: false,
      isError: false,
    };
    render(<ListConnectedDatabases />);

    expect(screen.queryByTestId('accelerate-section')).not.toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: /check latency/i }),
    ).toBeInTheDocument();
  });

  it('shows the section when a successful check reports high latency', () => {
    latencyState = {
      ...baseLatency,
      data: { latencies: [{ avgLatency: 300 }], rowId: 'row-1' },
      isSuccess: true,
      isError: false,
    };
    render(<ListConnectedDatabases />);

    expect(screen.getByTestId('accelerate-section')).toBeInTheDocument();
  });

  it('keeps the section hidden when a successful check reports only low latency', () => {
    latencyState = {
      ...baseLatency,
      data: { latencies: [{ avgLatency: 50 }], rowId: 'row-1' },
      isSuccess: true,
      isError: false,
    };
    render(<ListConnectedDatabases />);

    expect(screen.queryByTestId('accelerate-section')).not.toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: /check latency/i }),
    ).toBeInTheDocument();
  });

  it('toasts exactly once on error and keeps the section hidden (no render loop)', () => {
    latencyState = {
      ...baseLatency,
      data: {},
      isSuccess: false,
      isError: true,
    };
    render(<ListConnectedDatabases />);

    expect(screen.queryByTestId('accelerate-section')).not.toBeInTheDocument();
    expect(hasuraToastMock).toHaveBeenCalledTimes(1);
    expect(hasuraToastMock).toHaveBeenCalledWith(
      expect.objectContaining({ title: 'Could not fetch latency data!' }),
    );
  });
});
