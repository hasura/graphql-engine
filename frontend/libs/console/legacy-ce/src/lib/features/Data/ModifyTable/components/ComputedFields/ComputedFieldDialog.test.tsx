import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import type {
  MetadataTable,
  PostgresComputedField,
  Source,
} from '@hasura/shared/types';
import { ComputedFieldDialog } from './ComputedFieldDialog';

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

const addComputedField = vi.fn();
const dropComputedField = vi.fn();
const toast = vi.fn();
const errorToast = vi.fn();

vi.mock('@hasura/metadata/api', () => ({
  useAddComputedField: () => ({
    mutateAsync: addComputedField,
    isPending: false,
    error: null,
  }),
  useDropComputedField: () => ({
    mutateAsync: dropComputedField,
    isPending: false,
  }),
}));

vi.mock('@hasura/shared/ui', async (importOriginal) => {
  const actual = (await importOriginal()) as Record<string, unknown>;
  return {
    ...actual,
    hasuraToast: (...args: unknown[]) => toast(...args),
    showErrorNotification: (...args: unknown[]) => errorToast(...args),
  };
});

const source = { name: 'default', kind: 'postgres' } as Source;
const table = {
  table: { name: 'Album', schema: 'public' },
} as MetadataTable;

const original: PostgresComputedField = {
  name: 'full_title',
  definition: {
    function: { name: 'album_full_title', schema: 'public' },
    table_argument: 'album_row',
  },
  comment: 'original',
};

const setup = (onClose = vi.fn()) => {
  render(
    <ComputedFieldDialog
      source={source}
      table={table}
      functions={[]}
      computedField={original}
      onClose={onClose}
    />,
  );
  return { onClose };
};

// The dialog mounts its form after a short delay; a click on the footer before
// that is dropped, so wait for the form first.
const save = async () => {
  await waitFor(() =>
    expect(document.querySelector('input[name="name"]')).not.toBeNull(),
  );
  await userEvent.click(screen.getByRole('button', { name: 'Save' }));
};

describe('ComputedFieldDialog (edit)', () => {
  beforeEach(() => {
    [addComputedField, dropComputedField].forEach((m) => m.mockReset());
    toast.mockClear();
    errorToast.mockClear();
    dropComputedField.mockResolvedValue({});
  });

  it('replaces the computed field and closes on success', async () => {
    addComputedField.mockResolvedValue({});
    const { onClose } = setup();
    await save();

    await waitFor(() => expect(onClose).toHaveBeenCalled());
    expect(dropComputedField).toHaveBeenCalledWith(
      expect.objectContaining({ name: 'full_title' }),
    );
    expect(addComputedField).toHaveBeenCalledTimes(1);
    expect(toast).toHaveBeenCalledWith(
      expect.objectContaining({ type: 'success' }),
    );
  });

  it('re-adds the original computed field when adding the new one fails', async () => {
    addComputedField
      .mockRejectedValueOnce(new Error('invalid function'))
      .mockResolvedValueOnce({});
    const { onClose } = setup();
    await save();

    await waitFor(() => expect(addComputedField).toHaveBeenCalledTimes(2));
    expect(addComputedField).toHaveBeenLastCalledWith({
      source,
      table: table.table,
      ...original,
    });
    expect(errorToast).toHaveBeenCalledWith(
      expect.objectContaining({ title: 'Modifying computed field failed' }),
    );
    expect(errorToast).not.toHaveBeenCalledWith(
      expect.objectContaining({ title: 'Restoring computed field failed' }),
    );
    expect(onClose).not.toHaveBeenCalled();
  });

  it('reports when the original computed field cannot be restored', async () => {
    addComputedField.mockRejectedValue(new Error('invalid function'));
    setup();
    await save();

    await waitFor(() =>
      expect(errorToast).toHaveBeenCalledWith(
        expect.objectContaining({ title: 'Restoring computed field failed' }),
      ),
    );
  });

  it('does not restore when the drop itself fails', async () => {
    dropComputedField.mockRejectedValue(new Error('drop failed'));
    setup();
    await save();

    await waitFor(() =>
      expect(errorToast).toHaveBeenCalledWith(
        expect.objectContaining({ title: 'Modifying computed field failed' }),
      ),
    );
    expect(addComputedField).not.toHaveBeenCalled();
  });
});
