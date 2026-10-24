import { render, screen, fireEvent } from '@testing-library/react';
import { EditRowForm, EditRowFormProps } from './EditRowForm';

vi.mock('@hasura/metadata/data-source', async (importOriginal) => ({
  ...(await importOriginal<typeof import('@hasura/metadata/data-source')>()),
  useSupportedDataTypes: () => ({ data: undefined }),
}));

const row = {
  id: '31b789b0-acfe-43a8-bdaf-561313e8f82e',
  ts: '2026-10-22T10:48:04+00:00',
  f: 1.5,
  name: 'x',
};
const columns: EditRowFormProps['columns'] = [
  { name: 'id', dataType: 'uuid', consoleDataType: 'uuid' },
  {
    name: 'ts',
    dataType: 'timestamp with time zone',
    consoleDataType: 'timestamp',
  },
  { name: 'f', dataType: 'double precision', consoleDataType: 'float' },
  { name: 'name', dataType: 'text', consoleDataType: 'text' },
].map((column) => ({
  ...column,
  placeholder: '',
  editable: true,
  description: '',
})) as EditRowFormProps['columns'];

describe('EditRowForm existing Postgres values', () => {
  it('populates UUID, timestamp, float and text fields without marking them touched', () => {
    const onEditRow = vi.fn();
    render(
      <EditRowForm
        row={row}
        columns={columns}
        isSubmitting={false}
        isLoading={false}
        onEditRow={onEditRow}
      />,
    );
    Object.values(row).forEach((value) => {
      expect(screen.getByDisplayValue(String(value))).toHaveValue(
        String(value),
      );
    });
    fireEvent.click(screen.getByRole('button', { name: 'Save row' }));
    expect(onEditRow).toHaveBeenCalledWith({});
  });

  it('only submits the edited field, leaving UUID, timestamp and float untouched', () => {
    const onEditRow = vi.fn();
    render(
      <EditRowForm
        row={row}
        columns={columns}
        isSubmitting={false}
        isLoading={false}
        onEditRow={onEditRow}
      />,
    );
    fireEvent.change(screen.getByDisplayValue('x'), {
      target: { value: 'updated' },
    });
    fireEvent.click(screen.getByRole('button', { name: 'Save row' }));
    expect(onEditRow).toHaveBeenCalledWith({ name: 'updated' });
  });
});
