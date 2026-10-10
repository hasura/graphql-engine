import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import type { MetadataTable, Source } from '@hasura/shared/types';
import { EditTableColumnDialog } from './EditTableColumnDialog';

class ResizeObserverStub {
  observe() {
    /* no-op */
  }
  unobserve() {
    /* no-op */
  }
  disconnect() {
    /* no-op */
  }
}
globalThis.ResizeObserver =
  globalThis.ResizeObserver ?? (ResizeObserverStub as never);

const alterColumn = vi.fn().mockResolvedValue(true);
const updateTableConfiguration = vi.fn().mockResolvedValue(undefined);
let alterSupported = true;

vi.mock('@hasura/metadata/data-source', () => ({
  columnDataType: (t: string) => t,
  useSupportedDataTypes: () => ({ data: { integer: ['integer', 'bigint'] } }),
  useAlterColumn: () => ({
    mutateAsync: alterColumn,
    isPending: false,
    error: null,
    reset: vi.fn(),
  }),
  getDatabaseMethods: () => ({
    modify: alterSupported ? { alterColumn: () => undefined } : {},
  }),
}));

vi.mock('../../../ModifyTable/hooks', () => ({
  useUpdateTableConfiguration: () => ({
    updateTableConfiguration,
    isPending: false,
  }),
}));

const source = { name: 'default', kind: 'postgres' } as Source;
const table = {
  table: { name: 'Album', schema: 'public' },
  configuration: {},
} as MetadataTable;

const column = {
  name: 'Title',
  dataType: 'text',
  sqlType: 'text',
  consoleDataType: 'text' as const,
  nullable: true,
  defaultValue: null,
};

// Inputs aren't associated with their labels, so look them up by field name.
const findInput = (name: string) =>
  waitFor(() => {
    const el = document.querySelector<HTMLInputElement>(
      `input[name="${name}"]`,
    );
    if (!el) throw new Error(`input "${name}" not rendered`);
    return el;
  });

const setup = (onClose = vi.fn()) => {
  render(
    <EditTableColumnDialog
      source={source}
      table={table}
      isView={false}
      column={column}
      onClose={onClose}
    />,
  );
  return { onClose };
};

describe('EditTableColumnDialog', () => {
  beforeEach(() => {
    alterColumn.mockClear();
    updateTableConfiguration.mockClear();
    alterSupported = true;
  });

  it('alters the database column with the previous and next definitions', async () => {
    const { onClose } = setup();

    const name = await findInput('name');
    await userEvent.clear(name);
    await userEvent.type(name, 'title');
    await userEvent.type(await findInput('default'), "'untitled'");
    await userEvent.click(screen.getByRole('button', { name: 'Save' }));

    await waitFor(() => expect(alterColumn).toHaveBeenCalled());
    expect(alterColumn).toHaveBeenCalledWith({
      source: { name: 'default', kind: 'postgres' },
      table: table.table,
      previous: {
        name: 'Title',
        type: 'text',
        nullable: true,
        default: '',
        unique: false,
        uniqueConstraintName: undefined,
      },
      next: {
        name: 'title',
        type: 'text',
        nullable: true,
        default: "'untitled'",
        unique: false,
      },
    });
    // Metadata is untouched, so no configuration call.
    expect(updateTableConfiguration).not.toHaveBeenCalled();
    expect(onClose).toHaveBeenCalled();
  });

  it('only edits metadata when the driver cannot alter columns', async () => {
    alterSupported = false;
    setup();

    const fieldName = await findInput('custom_name');
    expect(document.querySelector('input[name="name"]')).toBeNull();
    await userEvent.type(fieldName, 'albumTitle');
    await userEvent.click(screen.getByRole('button', { name: 'Save' }));

    await waitFor(() =>
      expect(updateTableConfiguration).toHaveBeenCalledWith(
        expect.objectContaining({
          column_config: { Title: { custom_name: 'albumTitle' } },
        }),
      ),
    );
    expect(alterColumn).not.toHaveBeenCalled();
  });
});
