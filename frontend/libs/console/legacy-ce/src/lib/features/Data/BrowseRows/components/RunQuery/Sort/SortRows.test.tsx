import { beforeAll, describe, expect, it } from 'vitest';
import { render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { AppTheme, useConsoleForm } from '@hasura/shared/ui';
import { z } from 'zod';
import type { TableColumn } from '@hasura/metadata/data-source';
import { SortRows } from './SortRows';

beforeAll(() => {
  // Radix's Select relies on ResizeObserver, which jsdom does not implement.
  if (!('ResizeObserver' in globalThis)) {
    (globalThis as unknown as { ResizeObserver: unknown }).ResizeObserver =
      class {
        observe() {
          /* noop */
        }
        unobserve() {
          /* noop */
        }
        disconnect() {
          /* noop */
        }
      };
  }
});

const Harness = ({ tableColumns }: { tableColumns: TableColumn[] }) => {
  const { Form } = useConsoleForm({
    schema: z.object({ sorts: z.array(z.any()).optional() }),
    options: {
      defaultValues: {
        sorts: [{ column: '', type: 'asc' }],
      },
    },
  });

  return (
    <AppTheme>
      <Form onSubmit={() => undefined}>
        <SortRows name="sorts" columns={tableColumns} />
      </Form>
    </AppTheme>
  );
};

describe('SortRows column options', () => {
  it('does not mutate the columns array it is given', () => {
    const tableColumns = [
      { name: 'Zeta', dataType: 'string', consoleDataType: 'string' },
      { name: 'Alpha', dataType: 'string', consoleDataType: 'string' },
      { name: 'Mike', dataType: 'string', consoleDataType: 'string' },
    ] as unknown as TableColumn[];

    render(<Harness tableColumns={tableColumns} />);

    // Same contract as FilterRows: only the dropdown is alphabetical, the
    // caller's array keeps the introspection (table) order.
    expect(tableColumns.map((column) => column.name)).toEqual([
      'Zeta',
      'Alpha',
      'Mike',
    ]);
  });
});

describe('SortRows binary columns', () => {
  it('can not sort by a binary column', async () => {
    const tableColumns = [
      { name: 'id', dataType: 'integer', consoleDataType: 'integer' },
      { name: 'file', dataType: 'bytea', consoleDataType: 'binary' },
    ] as TableColumn[];

    render(<Harness tableColumns={tableColumns} />);
    await userEvent.click(screen.getByText('Select a column'));

    expect(screen.getByText('id')).toBeInTheDocument();
    expect(screen.queryByText('file')).not.toBeInTheDocument();
  });
});
