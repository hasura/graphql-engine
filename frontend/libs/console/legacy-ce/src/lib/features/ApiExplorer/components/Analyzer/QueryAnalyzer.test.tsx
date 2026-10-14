/**
 * Coverage for QueryAnalyzer after moving the mount props off the effect's
 * dependency array into a mount-capture ref. The analysis must run exactly once
 * for the query the dialog was opened with, must render the returned plan/sql,
 * must NOT re-run when the parent re-renders with fresh callback/header
 * identities, and must surface errors via clearAnalyse + the error notifier.
 */
import React from 'react';
import { render, screen, waitFor } from '@testing-library/react';
import { vi } from 'vitest';

const h = vi.hoisted(() => ({
  analyzeFetcher: vi.fn(),
  showError: vi.fn(),
}));

vi.mock('./utils', () => ({
  analyzeFetcher: h.analyzeFetcher,
}));

vi.mock('@hasura/metadata/api', () => ({
  useErrorNotification: () => h.showError,
}));

vi.mock('./RootFields', () => ({
  default: () => <div data-testid="root-fields" />,
}));

vi.mock('@hasura/shared/ui', () => ({
  hasuraToast: vi.fn(),
  Dialog: ({ children }: { children: React.ReactNode }) => (
    <div data-testid="dialog">{children}</div>
  ),
  PlainCodeBlock: ({ value }: { value: string }) => (
    <pre data-testid="plan">{value}</pre>
  ),
  SqlCodeBlock: ({ text }: { text: string }) => (
    <pre data-testid="sql">{text}</pre>
  ),
  Text: ({ children }: { children: React.ReactNode }) => (
    <span>{children}</span>
  ),
}));

import QueryAnalyzer from './QueryAnalyzer';

const props = (
  over: Partial<React.ComponentProps<typeof QueryAnalyzer>> = {},
) => ({
  mode: 'graphql' as const,
  query: { query: 'query { a }' } as any,
  headers: { h: '1' },
  clearAnalyse: vi.fn(),
  ...over,
});

beforeEach(() => {
  h.analyzeFetcher.mockReset();
  h.showError.mockReset();
});

describe('QueryAnalyzer', () => {
  it('runs the analysis once on mount and renders the result', async () => {
    h.analyzeFetcher.mockResolvedValue([{ plan: ['step-1'], sql: 'SELECT 1' }]);
    render(<QueryAnalyzer {...props()} />);

    await waitFor(() =>
      expect(screen.getByTestId('sql')).toHaveTextContent('SELECT 1'),
    );
    expect(screen.getByTestId('plan')).toHaveTextContent('step-1');
    expect(h.analyzeFetcher).toHaveBeenCalledTimes(1);
  });

  it('does NOT re-run the analysis when the parent re-renders with new callback/header identities', async () => {
    h.analyzeFetcher.mockResolvedValue([{ plan: ['p'], sql: 's' }]);
    const { rerender } = render(<QueryAnalyzer {...props()} />);
    await waitFor(() => expect(h.analyzeFetcher).toHaveBeenCalledTimes(1));

    // fresh clearAnalyse + fresh headers object (as happens on parent re-render)
    rerender(
      <QueryAnalyzer
        {...props({ clearAnalyse: vi.fn(), headers: { h: '1' } })}
      />,
    );
    rerender(
      <QueryAnalyzer
        {...props({ clearAnalyse: vi.fn(), headers: { h: '1' } })}
      />,
    );
    expect(h.analyzeFetcher).toHaveBeenCalledTimes(1);
  });

  it('calls clearAnalyse and the error notifier when the analysis fails', async () => {
    h.analyzeFetcher.mockRejectedValue(new Error('boom'));
    const clearAnalyse = vi.fn();
    render(<QueryAnalyzer {...props({ clearAnalyse })} />);

    await waitFor(() => expect(clearAnalyse).toHaveBeenCalledTimes(1));
    expect(h.showError).toHaveBeenCalledTimes(1);
  });
});
