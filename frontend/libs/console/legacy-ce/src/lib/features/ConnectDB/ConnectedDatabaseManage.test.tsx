/**
 * Coverage for ConnectedDatabaseManage's auto-redirect effect after adding the
 * missing `navigate` / `sourcesFromMetadata` dependencies. The redirect to the
 * connect page must: not fire while metadata is still loading, not fire when
 * sources exist, and fire exactly once when there are zero sources (the body is
 * idempotent via a module-level guard, so the added deps don't cause repeat
 * navigations).
 */
import React from 'react';
import { render } from '@testing-library/react';
import { vi } from 'vitest';

const h = vi.hoisted(() => ({
  data: undefined as unknown as any[] | undefined,
  isFetching: false,
  navigate: vi.fn(),
}));

vi.mock('@hasura/metadata/api', () => ({
  useMetadata: () => ({ data: h.data, isFetching: h.isFetching }),
}));

vi.mock('react-router', () => ({
  useNavigate: () => h.navigate,
}));

vi.mock('@hasura/shared/hooks', () => ({
  useDocumentTitle: () => undefined,
}));

vi.mock('../ManageAgents', () => ({ ManageAgents: () => <div /> }));
vi.mock('../ConnectDBRedesign', () => ({
  ListConnectedDatabases: () => <div />,
}));
vi.mock('./components/VPCBanner', () => ({ default: () => <div /> }));
vi.mock('./hooks/useVPCBannerVisibility', () => ({
  useVPCBannerVisibility: () => ({ show: false, dismiss: vi.fn() }),
}));
vi.mock(
  '../CloudOnboarding/NeonOnboardingWizard/components/NeonDashboardLink',
  () => ({ NeonDashboardLink: () => <div /> }),
);
vi.mock('@hasura/shared/analytics', () => ({
  Analytics: ({ children }: { children: React.ReactNode }) => <>{children}</>,
  REDACT_EVERYTHING: {},
}));
vi.mock('@hasura/shared/ui', () => ({
  Breadcrumbs: () => <div />,
  Button: ({ children }: { children: React.ReactNode }) => (
    <button>{children}</button>
  ),
  Collapsible: ({ children }: { children: React.ReactNode }) => (
    <div>{children}</div>
  ),
  IconTooltip: () => <div />,
  Separator: () => <div />,
  Text: ({ children }: { children: React.ReactNode }) => (
    <span>{children}</span>
  ),
}));

import ConnectedDatabaseManage from './ConnectedDatabaseManage';

beforeEach(() => {
  h.navigate.mockReset();
  h.data = undefined;
  h.isFetching = false;
});

describe('ConnectedDatabaseManage auto-redirect', () => {
  it('does not redirect while metadata is still fetching', () => {
    h.data = undefined;
    h.isFetching = true;
    render(<ConnectedDatabaseManage />);
    expect(h.navigate).not.toHaveBeenCalled();
  });

  it('does not redirect when sources exist', () => {
    h.data = [{ name: 'pg' } as any];
    h.isFetching = false;
    render(<ConnectedDatabaseManage />);
    expect(h.navigate).not.toHaveBeenCalled();
  });

  it('redirects exactly once when there are zero sources', () => {
    h.data = [];
    h.isFetching = false;
    const { rerender } = render(<ConnectedDatabaseManage />);
    expect(h.navigate).toHaveBeenCalledTimes(1);
    // re-render (new metadata object identity) must not redirect again
    rerender(<ConnectedDatabaseManage />);
    expect(h.navigate).toHaveBeenCalledTimes(1);
  });
});
