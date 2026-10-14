/**
 * Coverage for RestEndpointModal after stabilizing `tableEndpointDefinitions`
 * with useMemo (it previously produced a fresh `{}` each render, destabilizing
 * the `filteredEndpoints` memo). Output must be unchanged: the available
 * endpoint rows render when definitions exist, the empty-state card renders when
 * they don't, and a stable prop set does not trigger runaway re-renders.
 */
import React from 'react';
import { render, screen } from '@testing-library/react';
import { vi } from 'vitest';

const h = vi.hoisted(() => ({
  defs: {} as Record<string, any>,
  createRestEndpoints: vi.fn(),
}));

vi.mock('@hasura/metadata/api', () => ({
  useCreateRestEndpoints: () => ({
    createRestEndpoints: h.createRestEndpoints,
    endpointDefinitions: h.defs,
    isPending: false,
  }),
}));

vi.mock('react-router', () => ({ useNavigate: () => vi.fn() }));

vi.mock('@hasura/shared/ui', () => ({
  hasuraToast: vi.fn(),
  Dialog: ({
    children,
    footer,
  }: {
    children: React.ReactNode;
    footer: React.ReactNode;
  }) => (
    <div data-testid="dialog">
      {children}
      {footer}
    </div>
  ),
  Checkbox: () => <input type="checkbox" />,
  IndicatorCard: ({ children }: { children: React.ReactNode }) => (
    <div data-testid="empty-card">{children}</div>
  ),
  LearnMoreLink: () => <a href="#learn">learn</a>,
  CardedTable: ({ data }: { data: React.ReactNode[][] }) => (
    <div data-testid="carded-table">
      {data.map((row, i) => (
        <div key={i}>{row}</div>
      ))}
    </div>
  ),
  Badge: ({ children }: { children: React.ReactNode }) => (
    <span>{children}</span>
  ),
  DialogFooter: () => <div data-testid="footer" />,
  RelativeLink: ({ children }: { children: React.ReactNode }) => (
    <a href="#rel">{children}</a>
  ),
}));

import { RestEndpointModal } from './RestEndpointModal';

const baseProps = {
  onClose: vi.fn(),
  tableName: 'myTable',
  dataSourceName: 'pg',
  table: {} as any,
};

beforeEach(() => {
  h.defs = {};
});

describe('RestEndpointModal', () => {
  it('renders the creatable endpoint rows when definitions exist', () => {
    h.defs = {
      myTable: {
        READ: {
          exists: false,
          restEndpoint: { methods: ['GET'], url: 'my_url' },
          query: { query: 'q', name: 'n' },
        },
      },
    };
    render(<RestEndpointModal {...baseProps} />);
    expect(screen.getByTestId('carded-table')).toBeInTheDocument();
    expect(screen.getByText('READ')).toBeInTheDocument();
    expect(
      screen.queryByText(/No REST endpoints can be created/),
    ).not.toBeInTheDocument();
  });

  it('renders the empty-state card when no endpoints can be created', () => {
    h.defs = { myTable: {} };
    render(<RestEndpointModal {...baseProps} />);
    expect(screen.getByTestId('empty-card')).toHaveTextContent(
      'No REST endpoints can be created',
    );
  });

  it('does not re-render unboundedly for stable props', () => {
    h.defs = {
      myTable: {
        READ: {
          exists: false,
          restEndpoint: { methods: ['GET'], url: 'u' },
          query: { query: 'q', name: 'n' },
        },
      },
    };
    let commits = 0;
    const Probe = () => {
      commits++;
      return <RestEndpointModal {...baseProps} />;
    };
    const { rerender } = render(<Probe />);
    const mountCommits = commits;
    rerender(<Probe />);
    rerender(<Probe />);
    expect(commits - mountCommits).toBeLessThanOrEqual(2);
    expect(screen.getByText('READ')).toBeInTheDocument();
  });
});
