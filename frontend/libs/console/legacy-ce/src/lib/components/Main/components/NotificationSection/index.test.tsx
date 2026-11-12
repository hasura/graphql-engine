import { beforeAll, beforeEach, describe, expect, it, vi } from 'vitest';
import { Profiler } from 'react';
import { render, screen } from '@testing-library/react';

vi.mock('lottie-react', () => ({
  __esModule: true,
  default: () => null,
  LottieSvg: () => null,
}));

const appContext = {
  serverVersion: 'v2.0.0',
  latestServerVersion: { latest: 'v2.0.0', prerelease: 'v2.0.0' },
};
vi.mock('@hasura/shared/context', () => ({
  __esModule: true,
  useAppContext: () => appContext,
  useAuthContext: () => ({ hasuraUserId: 'admin' }),
}));

const getConsoleNotification = vi.fn();
const refreshCatalogState = vi.fn();
const updateConsoleNotificationsState = vi.fn();
const setPreReleaseNotificationOptOut = vi.fn();

const makeConsoleState = (read: string[]) => ({
  console_notifications: {
    admin: { read, showBadge: true, date: '2024-01-01' },
  },
});

let storeState: { consoleState: any; notifications: any[] };

vi.mock('../../../../telemetry', () => ({
  __esModule: true,
  useCatalogState: () => ({
    refreshCatalogState,
    setPreReleaseNotificationOptOut,
  }),
  useConsoleNotifications: () => ({
    updateConsoleNotificationsState,
    getConsoleNotification,
  }),
  useTelemetryStore: () => storeState,
}));

import HasuraNotifications from './index';

const notificationsOf = (n: number) =>
  Array.from({ length: n }, (_v, i) => ({
    id: i + 1,
    subject: `s${i}`,
    content: `c${i}`,
    type: 'security',
    start_date: null,
  }));

beforeAll(() => {
  if (!('ResizeObserver' in globalThis)) {
    (globalThis as any).ResizeObserver = class {
      observe() {
        /* noop */
      }
      unobserve() {
        /* noop */
      }
      disconnect() {
        /* noop */
      }
    };
  }
  if (!Element.prototype.hasPointerCapture) {
    Element.prototype.hasPointerCapture = () => false;
  }
  if (!Element.prototype.scrollIntoView) {
    Element.prototype.scrollIntoView = () => undefined;
  }
});

beforeEach(() => {
  vi.clearAllMocks();
  window.localStorage.clear();
  storeState = {
    consoleState: makeConsoleState(['1', '2']),
    notifications: notificationsOf(5),
  };
});

describe('HasuraNotifications lifecycle', () => {
  it('fetches console notifications exactly once on mount', () => {
    render(<HasuraNotifications />);
    expect(getConsoleNotification).toHaveBeenCalledTimes(1);
  });

  it('does not refetch notifications on subsequent re-renders', () => {
    const { rerender } = render(<HasuraNotifications />);
    rerender(<HasuraNotifications />);
    rerender(<HasuraNotifications />);
    expect(getConsoleNotification).toHaveBeenCalledTimes(1);
  });

  it('shows the computed unread count badge on initial render', () => {
    render(<HasuraNotifications />);
    // 5 notifications minus 2 already read = 3 unread
    expect(screen.getByText('3')).toBeInTheDocument();
  });

  it('resets the unread count when the read state changes (adjust-in-render)', () => {
    const { rerender } = render(<HasuraNotifications />);
    expect(screen.getByText('3')).toBeInTheDocument();

    // only one item read now -> 5 - 1 = 4 unread
    storeState = {
      ...storeState,
      consoleState: makeConsoleState(['1']),
    };
    rerender(<HasuraNotifications />);

    expect(screen.getByText('4')).toBeInTheDocument();
    // the mount-only fetch is still not re-triggered
    expect(getConsoleNotification).toHaveBeenCalledTimes(1);
  });

  it('settles in a bounded number of commits (no runaway render)', () => {
    let commitCount = 0;
    const { rerender } = render(
      <Profiler id="notifications" onRender={() => (commitCount += 1)}>
        <HasuraNotifications />
      </Profiler>,
    );
    const afterMount = commitCount;

    storeState = {
      ...storeState,
      consoleState: makeConsoleState(['1']),
    };
    rerender(
      <Profiler id="notifications" onRender={() => (commitCount += 1)}>
        <HasuraNotifications />
      </Profiler>,
    );

    // one external change -> at most the triggering render plus the collapsed
    // in-render state adjustment.
    expect(commitCount - afterMount).toBeLessThanOrEqual(3);
  });
});
