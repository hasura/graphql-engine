import { fireEvent, render, screen, within } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import ResultTable from './ResultTable';

// jsdom has no 2D canvas; measure text as 8px per character so column widths
// follow the length of their content
beforeEach(() => {
  vi.spyOn(HTMLCanvasElement.prototype, 'getContext').mockReturnValue({
    font: '',
    measureText: (text: string) => ({ width: text.length * 8 }),
  } as unknown as CanvasRenderingContext2D);
  // nor a ResizeObserver, which the Radix dialog uses
  vi.stubGlobal(
    'ResizeObserver',
    class {
      observe() {
        /* no-op */
      }
      unobserve() {
        /* no-op */
      }
      disconnect() {
        /* no-op */
      }
    },
  );
});

afterEach(() => {
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
});

const renderResult = (headers: string[], rows: string[][]) =>
  render(
    <Theme>
      <ResultTable headers={headers} rows={rows} />
    </Theme>,
  );

describe('ResultTable', () => {
  it('shows every column when a column name repeats (#8338)', () => {
    const long = 'x'.repeat(20);
    renderResult(['id', 'id'], [['1', long]]);

    const headers = screen.getAllByRole('rowheader', { name: 'id' });
    expect(headers).toHaveLength(2);
    expect(screen.getByText('1')).toBeInTheDocument();
    expect(screen.getByText(long)).toBeInTheDocument();

    // each column is sized from its own values, not from its namesake's
    expect(headers[0]).toHaveStyle({ width: '78px' });
    expect(headers[1]).toHaveStyle({ width: '178px' });
  });

  it('cuts long values to the column width and shows them in full on hover', () => {
    const long = 'x'.repeat(100);
    renderResult(
      ['id', 'data'],
      [
        ['1', long],
        [null as unknown as string, 'a'],
      ],
    );

    // 100 * 8 + 18 is over the 600px cap
    const cell = screen.getByText(long);
    expect(cell).toHaveClass('truncate');
    expect(cell).toHaveStyle({ maxWidth: '600px' });
    expect(cell).toHaveAttribute('title', long);

    // NULL is a marker, not a value to show on hover
    expect(screen.getByText('NULL').closest('div')).not.toHaveAttribute(
      'title',
    );
  });

  it('opens a row in full, in the order of the result', () => {
    const long = 'x'.repeat(100);
    renderResult(
      ['id', 'id', 'payload', 'note'],
      [
        ['1', 'a', 'first', 'b'],
        ['2', long, '{"nested":{"a":1}}', null as unknown as string],
      ],
    );

    fireEvent.click(screen.getAllByRole('button', { name: 'Open row' })[1]);

    const dialog = within(screen.getByRole('dialog'));
    expect(dialog.getByText('Result Row')).toBeInTheDocument();
    // both `id` columns, with the full value of the truncated cell
    expect(dialog.getAllByText('id')).toHaveLength(2);
    expect(dialog.getByTestId('result-value-0')).toHaveTextContent(/^2$/);
    expect(dialog.getByTestId('result-value-1')).toHaveTextContent(long);
    // JSON is formatted
    expect(dialog.getByTestId('result-value-2')).toHaveTextContent('"nested"');
    expect(dialog.getByTestId('result-value-3')).toHaveTextContent('NULL');
  });

  it('sizes a column from its widest value, however far down it is', () => {
    const rows = Array.from({ length: 5000 }, (_, i) => [String(i % 10)]);
    rows[4321] = ['x'.repeat(40)];
    renderResult(['id'], rows);

    // 40 * 8 + 18
    expect(screen.getByRole('rowheader', { name: 'id' })).toHaveStyle({
      width: '338px',
    });
  });

  it('does not read a dotted column name as a nested path', () => {
    renderResult(['a.b'], [['value']]);

    expect(screen.getByRole('rowheader', { name: 'a.b' })).toBeInTheDocument();
    expect(screen.getByText('value')).toBeInTheDocument();
  });
});
