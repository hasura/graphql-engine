import { render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import type { Source } from '@hasura/shared/types';
import { ForeignKeyEditForm } from './ForeignKeyEditForm';

const alter = vi.fn();

vi.mock('@hasura/metadata/data-source', () => ({
  useAlterForeignKey: () => ({ mutate: alter, isPending: false }),
  getDatabaseMethods: () => ({
    config: { getViolationActions: () => ['restrict', 'cascade'] },
  }),
}));

// Keep the real helpers (fkToFormValues) but stub the UI so we
// can drive onSubmit deterministically without Radix Select interaction.
vi.mock('./ForeignKeyForm', async (importOriginal) => {
  const actual = (await importOriginal()) as Record<string, unknown>;
  return {
    ...actual,
    ForeignKeyForm: (props: {
      onSubmit: (v: unknown) => void;
      onCancel?: () => void;
    }) => (
      <div>
        <button
          onClick={() =>
            props.onSubmit({
              referenceTable: { name: 'customers', schema: 'public' },
              columnMappings: [{ from: 'customer_id', to: 'id' }],
              onUpdate: 'cascade',
              onDelete: 'restrict',
            })
          }
        >
          submit
        </button>
      </div>
    ),
  };
});

const source = { name: 'default', kind: 'postgres' } as Source;
const table = { name: 'orders', schema: 'public' };
const existingFk = {
  name: 'orders_customer_id_fkey',
  from: { table, columns: ['customer_id'] },
  to: { table: { name: 'customers', schema: 'public' }, columns: ['id'] },
  onUpdate: 'restrict' as const,
  onDelete: 'restrict' as const,
};

describe('ForeignKeyEditForm', () => {
  beforeEach(() => alter.mockClear());

  it('alters the foreign key with the existing constraint name and mapped columns', async () => {
    render(
      <ForeignKeyEditForm
        source={source}
        table={table}
        foreignKey={existingFk}
        onDone={vi.fn()}
      />,
    );
    await userEvent.click(screen.getByRole('button', { name: 'submit' }));

    expect(alter).toHaveBeenCalledWith(
      expect.objectContaining({
        source: { name: 'default', kind: 'postgres' },
        constraintName: 'orders_customer_id_fkey',
        from: { table, columns: ['customer_id'] },
        to: {
          table: { name: 'customers', schema: 'public' },
          columns: ['id'],
        },
        onUpdate: 'cascade',
        onDelete: 'restrict',
      }),
    );
  });
});
