import { beforeEach, describe, expect, it, vi } from 'vitest';
import { Profiler } from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import { LS_KEYS } from '@hasura/shared/types';
import { getLSItem } from '@hasura/shared/utils';

vi.mock('lottie-react', () => ({
  __esModule: true,
  default: () => null,
  LottieSvg: () => null,
}));

// SCSS modules are stubbed to an empty string; echo class keys back so we can
// select the close affordance deterministically.
vi.mock('../Main.module.scss', () => ({
  __esModule: true,
  default: new Proxy({}, { get: (_t, key: string) => key }),
}));

const appContext = {
  envVars: { consoleType: 'oss' },
  serverVersion: 'v2.0.0',
  latestServerVersion: { latest: 'v2.1.0', prerelease: 'v2.1.0' },
};

vi.mock('@hasura/shared/context', () => ({
  __esModule: true,
  useAppContext: () => appContext,
}));

const setPreReleaseNotificationOptOut = vi.fn();
vi.mock('../../../telemetry/useCatalogState', () => ({
  __esModule: true,
  default: () => ({ setPreReleaseNotificationOptOut }),
}));

import { UpdateVersion } from './UpdateVersion';

const consoleState = { disablePreReleaseUpdateNotifications: true } as any;

beforeEach(() => {
  window.localStorage.clear();
  vi.clearAllMocks();
  appContext.envVars.consoleType = 'oss';
});

describe('UpdateVersion banner', () => {
  it('shows the update banner on the initial render (lazy-seeded state)', () => {
    render(<UpdateVersion consoleState={consoleState} />);
    expect(screen.getByText(/A new server version/i)).toBeInTheDocument();
    expect(screen.getByText('v2.1.0')).toBeInTheDocument();
  });

  it('renders nothing when the console type is not oss', () => {
    appContext.envVars.consoleType = 'cloud';
    const { container } = render(<UpdateVersion consoleState={consoleState} />);
    expect(container).toBeEmptyDOMElement();
  });

  it('renders nothing when the last-closed version already matches', () => {
    // getLSItem returns the raw stored string; store the bare version to match.
    window.localStorage.setItem(LS_KEYS.versionUpdateCheckLastClosed, 'v2.1.0');
    const { container } = render(<UpdateVersion consoleState={consoleState} />);
    expect(container).toBeEmptyDOMElement();
  });

  it('closes the banner and records the dismissed version on click', () => {
    render(<UpdateVersion consoleState={consoleState} />);
    expect(screen.getByText(/A new server version/i)).toBeInTheDocument();

    const close = document.querySelector('.updateBannerClose') as HTMLElement;
    fireEvent.click(close);

    expect(screen.queryByText(/A new server version/i)).not.toBeInTheDocument();
    expect(getLSItem(LS_KEYS.versionUpdateCheckLastClosed)).toBe('v2.1.0');
  });

  it('does not re-render in a runaway loop', () => {
    let commitCount = 0;
    render(
      <Profiler id="update-version" onRender={() => (commitCount += 1)}>
        <UpdateVersion consoleState={consoleState} />
      </Profiler>,
    );
    // Lazy initial state means a single mount commit, no effect-driven second
    // render churn.
    expect(commitCount).toBeLessThanOrEqual(1);
  });
});
