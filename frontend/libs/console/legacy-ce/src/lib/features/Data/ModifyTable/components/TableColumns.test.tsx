import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import type { MetadataTable, Source } from '@hasura/shared/types';
import { TableColumns } from './TableColumns';

const dropColumn = vi.fn().mockResolvedValue(true);
const confirmSpy = vi.fn(
  ({ onConfirm }: { onConfirm: () => Promise<boolean> }) => onConfirm(),
);
let dropSupported = true;

const albumTable = { name: 'Album', schema: 'public' };

vi.mock('@hasura/metadata/data-source', () => ({
  columnDataType: (t: string) => t,
  useTableColumns: () => ({
    data: [
      {
        name: 'AlbumId',
        dataType: 'integer',
        sqlType: 'integer',
        consoleDataType: 'integer',
        nullable: false,
        isPrimaryKey: true,
        defaultValue: null,
      },
      {
        name: 'ArtistId',
        dataType: 'integer',
        sqlType: 'integer',
        consoleDataType: 'integer',
        nullable: false,
        defaultValue: null,
      },
    ],
    isLoading: false,
    error: null,
  }),
  useTableUniqueKeys: () => ({
    data: [
      { constraintName: 'uq_artist', columns: ['ArtistId'] },
      { constraintName: 'uq_other', columns: ['AlbumId'] },
    ],
  }),
  useTablePrimaryKey: () => ({
    data: { constraintName: 'PK_Album', columns: ['AlbumId'] },
  }),
  useTableForeignKeys: () => ({
    data: [
      {
        name: 'FK_AlbumArtistId',
        from: {
          table: { name: 'Album', schema: 'public' },
          columns: ['ArtistId'],
        },
        to: {
          table: { name: 'Artist', schema: 'public' },
          columns: ['ArtistId'],
        },
      },
      // Incoming FK from another table: not ours to recreate.
      {
        name: 'FK_TrackAlbumId',
        from: {
          table: { name: 'Track', schema: 'public' },
          columns: ['AlbumId'],
        },
        to: {
          table: { name: 'Album', schema: 'public' },
          columns: ['ArtistId'],
        },
      },
    ],
  }),
  useDropColumn: () => ({ mutateAsync: dropColumn }),
  getDatabaseMethods: () => ({
    introspection: { getUniqueKeys: vi.fn(), getPrimaryKey: vi.fn() },
    modify: dropSupported ? { dropColumn: vi.fn() } : {},
  }),
}));

vi.mock('@hasura/shared/ui', async (importOriginal) => {
  const actual = (await importOriginal()) as Record<string, unknown>;
  return { ...actual, useDestructiveConfirm: () => confirmSpy };
});

vi.mock('./EditTableColumnDialog/EditTableColumnDialog', () => ({
  EditTableColumnDialog: () => null,
}));
vi.mock('./AddColumnDialog', () => ({ AddColumnDialog: () => null }));

const source = { name: 'default', kind: 'postgres' } as Source;
const table = { table: albumTable, configuration: {} } as MetadataTable;

describe('ModifyTable TableColumns', () => {
  beforeEach(() => {
    dropColumn.mockClear();
    confirmSpy.mockClear();
    dropSupported = true;
  });

  it('removes a column with the constraints of this table that involve it', async () => {
    render(<TableColumns source={source} table={table} isView={false} />);
    await userEvent.click(
      screen.getByRole('button', { name: 'Remove column ArtistId' }),
    );

    expect(confirmSpy).toHaveBeenCalledWith(
      expect.objectContaining({
        resourceName: 'ArtistId',
        resourceType: 'Column',
      }),
    );
    await waitFor(() => expect(dropColumn).toHaveBeenCalled());
    expect(dropColumn).toHaveBeenCalledWith({
      source: { name: 'default', kind: 'postgres' },
      table: albumTable,
      column: {
        name: 'ArtistId',
        type: 'integer',
        nullable: false,
        default: null,
      },
      primaryKey: undefined,
      uniqueKeys: [{ constraintName: 'uq_artist', columns: ['ArtistId'] }],
      foreignKeys: [expect.objectContaining({ name: 'FK_AlbumArtistId' })],
    });
  });

  it('passes the primary key when removing one of its columns', async () => {
    render(<TableColumns source={source} table={table} isView={false} />);
    await userEvent.click(
      screen.getByRole('button', { name: 'Remove column AlbumId' }),
    );
    await waitFor(() => expect(dropColumn).toHaveBeenCalled());
    expect(dropColumn).toHaveBeenCalledWith(
      expect.objectContaining({
        primaryKey: { constraintName: 'PK_Album', columns: ['AlbumId'] },
        foreignKeys: [],
      }),
    );
  });

  it('hides Remove for views and unsupported drivers', () => {
    const { rerender } = render(
      <TableColumns source={source} table={table} isView />,
    );
    expect(
      screen.queryByRole('button', { name: /remove column/i }),
    ).not.toBeInTheDocument();

    dropSupported = false;
    rerender(<TableColumns source={source} table={table} isView={false} />);
    expect(
      screen.queryByRole('button', { name: /remove column/i }),
    ).not.toBeInTheDocument();
  });
});
