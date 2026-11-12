import { render, screen } from '@testing-library/react';
import { vi } from 'vitest';

const h = vi.hoisted(() => ({ initServerVersion: vi.fn() }));

vi.mock('@hasura/metadata/api', () => ({
  useServerVersion: () => ({
    serverVersion: '1.0.0',
    latestServerVersion: { latest: '', prerelease: '' },
    featuresCompatibility: {},
    initServerVersion: h.initServerVersion,
  }),
}));

vi.mock('../../Globals', () => ({
  default: { urlPrefix: '', dataApiUrl: '' },
}));

import AppProvider from './AppProvider';

beforeEach(() => {
  h.initServerVersion.mockReset();
  (window as any).__env = {};
});

describe('AppProvider', () => {
  it('renders children and kicks off initServerVersion exactly once on mount', () => {
    render(
      <AppProvider>
        <div>child-content</div>
      </AppProvider>,
    );
    expect(screen.getByText('child-content')).toBeInTheDocument();
    expect(h.initServerVersion).toHaveBeenCalledTimes(1);
  });

  it('does not re-invoke initServerVersion on re-render', () => {
    const { rerender } = render(
      <AppProvider>
        <div>a</div>
      </AppProvider>,
    );
    rerender(
      <AppProvider>
        <div>b</div>
      </AppProvider>,
    );
    rerender(
      <AppProvider>
        <div>c</div>
      </AppProvider>,
    );
    expect(h.initServerVersion).toHaveBeenCalledTimes(1);
  });
});
