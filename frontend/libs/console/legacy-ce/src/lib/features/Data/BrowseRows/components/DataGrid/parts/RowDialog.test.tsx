import { render, screen, within } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { TableColumn } from '@hasura/metadata/data-source';
import { RowDialog } from './RowDialog';

const columns: TableColumn[] = [
  { name: 'title', dataType: 'text', consoleDataType: 'text' },
  {
    name: 'id',
    dataType: 'uuid',
    consoleDataType: 'uuid',
    isPrimaryKey: true,
  },
  { name: 'active', dataType: 'boolean', consoleDataType: 'boolean' },
  { name: 'data', dataType: 'jsonb', consoleDataType: 'json' },
  {
    name: 'tags',
    dataType: 'text[]',
    consoleDataType: 'array',
    arrayItemType: 'string',
  },
  { name: 'note', dataType: 'text', consoleDataType: 'text' },
  { name: 'file', dataType: 'bytea', consoleDataType: 'binary' },
];

const row = {
  id: '31b789b0-acfe-43a8-bdaf-561313e8f82e',
  title: 'Line one\nLine two',
  active: true,
  data: '{"nested":{"a":1}}',
  tags: ['a', null],
  note: null,
  file: '\\x48656c6c6f',
  extra: 7,
};

const value = (column: string) => screen.getByTestId(`row-value-${column}`);

describe('RowDialog', () => {
  beforeAll(() => {
    global.ResizeObserver ??= class {
      observe() {
        /* noop */
      }
      unobserve() {
        /* noop */
      }
      disconnect() {
        /* noop */
      }
    } as unknown as typeof ResizeObserver;
  });

  const renderRowDialog = () =>
    render(
      <Theme>
        <RowDialog row={row} columns={columns} onClose={vi.fn()} />
      </Theme>,
    );

  it('is a read-only view, without form inputs', () => {
    renderRowDialog();
    expect(screen.queryByRole('textbox')).not.toBeInTheDocument();
  });

  it('lists the primary keys first, then the other columns by name', () => {
    renderRowDialog();
    expect(
      screen
        .getAllByTestId(/^row-value-/)
        .map((element) => element.dataset.testid?.replace('row-value-', '')),
    ).toEqual([
      'id',
      'active',
      'data',
      'extra',
      'file',
      'note',
      'tags',
      'title',
    ]);
    expect(screen.getByLabelText('Primary key')).toBeInTheDocument();
  });

  it('renders each value with a component suited to its data type', () => {
    renderRowDialog();
    expect(value('id')).toHaveTextContent(row.id);
    // line breaks are kept
    expect(value('title')).toHaveTextContent('Line one\nLine two', {
      normalizeWhitespace: false,
    });
    expect(value('active')).toHaveTextContent('true');
    // the JSON text is parsed and pretty printed
    expect(value('data')).toHaveTextContent('"nested"');
    expect(
      within(value('tags'))
        .getAllByText(/./)
        .map((chip) => chip.textContent),
    ).toEqual(['a', 'NULL']);
    expect(value('note')).toHaveTextContent('NULL');
    expect(value('extra')).toHaveTextContent('7');
  });

  it('offers binary values as a download instead of showing them', () => {
    renderRowDialog();
    expect(value('file')).toHaveTextContent('Download (5 B)');
    expect(value('file')).not.toHaveTextContent('48656c6c6f');
  });
});
