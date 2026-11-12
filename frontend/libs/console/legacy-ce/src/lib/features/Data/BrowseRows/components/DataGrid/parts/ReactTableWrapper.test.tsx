import { render, screen, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { MemoryRouter, Route, Routes, useLocation } from 'react-router';
import { Theme } from '@radix-ui/themes';
import { TableColumn } from '@hasura/metadata/data-source';
import { ReactTableWrapper } from './ReactTableWrapper';

// renders where "Clone row" navigates, with the router state it passes
const InsertRoute = () => {
  const location = useLocation();
  return (
    <output data-testid="insert-route">
      {JSON.stringify({
        path: `${location.pathname}${location.search}`,
        state: location.state,
      })}
    </output>
  );
};

const rows = [{ id: 1, title: 'a' }];

const idColumn: TableColumn = {
  name: 'id',
  dataType: 'integer',
  consoleDataType: 'integer',
};
const titleColumn: TableColumn = {
  name: 'title',
  dataType: 'text',
  consoleDataType: 'text',
};

const openRowMenu = async (tableColumns: TableColumn[]) => {
  render(
    <Theme>
      <MemoryRouter>
        <Routes>
          <Route
            path="/"
            element={
              <ReactTableWrapper
                rows={rows}
                tableColumns={tableColumns}
                isRowsSelectionEnabled
                onRowsSelect={vi.fn()}
                onRowDelete={vi.fn()}
                source={{ name: 'db', kind: 'postgres', tables: [] } as never}
                table={{ schema: 'public', name: 'album' }}
                disabled={false}
                selectedRows={{}}
              />
            }
          />
          <Route path="*" element={<InsertRoute />} />
        </Routes>
      </MemoryRouter>
    </Theme>,
  );

  const [menuButton] = within(screen.getByTestId('@table-row-0')).getAllByRole(
    'button',
  );
  await userEvent.click(menuButton);
  return (await screen.findAllByRole('menuitem')).map(
    (item) => item.textContent,
  );
};

describe('ReactTableWrapper row options', () => {
  beforeAll(() => {
    // Radix menus rely on DOM APIs jsdom does not implement
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
    Element.prototype.hasPointerCapture ??= () => false;
    Element.prototype.releasePointerCapture ??= () => undefined;
    Element.prototype.scrollIntoView ??= () => undefined;
  });

  it('can edit and delete rows of a table with a primary key', async () => {
    expect(
      await openRowMenu([{ ...idColumn, isPrimaryKey: true }, titleColumn]),
    ).toEqual(['Open', 'Edit row', 'Clone row', 'Delete']);
  });

  it('hides edit and delete without a primary key', async () => {
    expect(await openRowMenu([idColumn, titleColumn])).toEqual([
      'Open',
      'Clone row',
    ]);
  });

  it('hides edit and delete when a key column is not in the rows', async () => {
    // e.g. the hidden `rowid` key of a CockroachDB table
    const rowid: TableColumn = {
      name: 'rowid',
      dataType: 'bigint',
      consoleDataType: 'integer',
      isPrimaryKey: true,
    };
    expect(await openRowMenu([rowid, idColumn, titleColumn])).toEqual([
      'Open',
      'Clone row',
    ]);
  });

  it('clones a row in the Insert Row tab, prefilled through the router state', async () => {
    await openRowMenu([{ ...idColumn, isPrimaryKey: true }, titleColumn]);
    await userEvent.click(screen.getByRole('menuitem', { name: 'Clone row' }));

    expect(
      JSON.parse(screen.getByTestId('insert-route').textContent ?? ''),
    ).toEqual({
      path: `/data/manage/source/db/table/insert?table=${encodeURIComponent(
        JSON.stringify({ schema: 'public', name: 'album' }),
      )}`,
      state: { cloneRow: { id: 1, title: 'a' } },
    });
  });
});

describe('ReactTableWrapper binary columns', () => {
  const fileColumn: TableColumn = {
    name: 'file',
    dataType: 'bytea',
    consoleDataType: 'binary',
  };

  const renderBinaryTable = (setSorting = vi.fn()) =>
    render(
      <Theme>
        <MemoryRouter>
          <ReactTableWrapper
            rows={[{ id: 7, file: '\\x48656c6c6f' }]}
            tableColumns={[{ ...idColumn, isPrimaryKey: true }, fileColumn]}
            isRowsSelectionEnabled
            onRowsSelect={vi.fn()}
            onRowDelete={vi.fn()}
            source={{ name: 'db', kind: 'postgres', tables: [] } as never}
            table={{ schema: 'public', name: 'album' }}
            disabled={false}
            selectedRows={{}}
            sort={{ sorting: [], setSorting }}
          />
        </MemoryRouter>
      </Theme>,
    );

  it('offers the value as a download instead of showing it', () => {
    renderBinaryTable();
    const download = screen.getByTestId('@download-file');

    expect(download).toHaveTextContent('Download (5 B)');
    expect(screen.queryByText(/48656c6c6f/)).not.toBeInTheDocument();
  });

  it('downloads the decoded bytes, named after the table and the row', async () => {
    const createObjectURL = vi.fn((_: Blob) => 'blob:file');
    const revokeObjectURL = vi.fn();
    Object.assign(URL, { createObjectURL, revokeObjectURL });
    const click = vi
      .spyOn(HTMLAnchorElement.prototype, 'click')
      .mockImplementation(function (this: HTMLAnchorElement) {
        expect(this.download).toBe('public.album-file-7.bin');
        expect(this.href).toBe('blob:file');
      });

    renderBinaryTable();
    await userEvent.click(screen.getByTestId('@download-file'));

    expect(click).toHaveBeenCalledOnce();
    const blob = createObjectURL.mock.calls[0][0];
    expect(new TextDecoder().decode(await blob.arrayBuffer())).toBe('Hello');
    expect(revokeObjectURL).toHaveBeenCalledWith('blob:file');
    click.mockRestore();
  });

  it('can not be sorted', async () => {
    const setSorting = vi.fn();
    renderBinaryTable(setSorting);

    await userEvent.click(screen.getByText('file'));
    expect(setSorting).not.toHaveBeenCalled();

    // other columns still sort
    await userEvent.click(screen.getByText('id'));
    expect(setSorting).toHaveBeenCalled();
  });
});
