import { render, screen, fireEvent } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { EventInvocation } from '@hasura/shared/types';
import RedeliverEventLogsTable from './RedeliverEventLogsTable';

const logs = [
  { status: 200, request: { a: 1 }, response: { ok: true } },
  { status: 404, request: { a: 2 }, response: { ok: false } },
] as unknown as EventInvocation[];

const renderTable = (
  props: Partial<React.ComponentProps<typeof RedeliverEventLogsTable>> = {},
) => {
  const setExpandedRowIndex = vi.fn();
  render(
    <Theme>
      <RedeliverEventLogsTable
        logs={logs}
        invocationColumns={['status']}
        expandedRowIndex={null}
        setExpandedRowIndex={setExpandedRowIndex}
        {...props}
      />
    </Theme>,
  );
  return { setExpandedRowIndex };
};

describe('RedeliverEventLogsTable', () => {
  it('renders a row per log', () => {
    renderTable();
    expect(screen.getByText('200')).toBeInTheDocument();
    expect(screen.getByText('404')).toBeInTheDocument();
  });

  it('calls setExpandedRowIndex with the row index when the expander is clicked', () => {
    const { setExpandedRowIndex } = renderTable();

    const expanders = screen.getAllByTitle('Expand row');
    fireEvent.click(expanders[0]);

    expect(setExpandedRowIndex).toHaveBeenCalledWith(0);
  });

  it('shows invocation details for the expanded row', () => {
    renderTable({ expandedRowIndex: 0 });
    // InvocationLogDetails renders Request/Response tabs for the expanded row
    expect(screen.getAllByText('Request').length).toBeGreaterThan(0);
  });
});
