import { render, screen } from '@testing-library/react';
import { useConsoleForm } from '@hasura/shared/ui';
import { z } from 'zod';
import type { TableColumn, Operator } from '@hasura/metadata/data-source';
import { FilterRows } from './FilterRows';

const columns = [
  { name: 'FirstName', dataType: 'string', consoleDataType: 'string' },
  { name: 'ID', dataType: 'number', consoleDataType: 'number' },
] as unknown as TableColumn[];

const operators = [
  { name: '_eq', value: '_eq' },
  { name: '_neq', value: '_neq' },
] as unknown as Operator[];

const Harness = ({
  initialFilters,
  tableColumns = columns,
}: {
  initialFilters?: { column: string; operator: string; value: string }[];
  tableColumns?: TableColumn[];
}) => {
  const { Form } = useConsoleForm({
    schema: z.object({ filters: z.array(z.any()).optional() }),
    options: {
      defaultValues: {
        filters: [{ column: '', operator: '', value: '' }],
      },
    },
  });

  return (
    <Form onSubmit={() => undefined}>
      <FilterRows
        name="filters"
        columns={tableColumns}
        operators={operators}
        initialFilters={initialFilters}
      />
    </Form>
  );
};

describe('FilterRows initial filters effect', () => {
  it('renders without initial filters (effect is a no-op)', () => {
    expect(() => render(<Harness />)).not.toThrow();
    expect(screen.getByTestId('filters-filter-rows')).toBeInTheDocument();
  });

  it('runs the initial-filters effect without entering a render loop', () => {
    // The effect now depends on [initialFilters, update]. `update` is a stable
    // react-hook-form callback, so providing initialFilters must not cause a
    // runaway re-render (React would throw "Too many re-renders").
    expect(() =>
      render(
        <Harness
          initialFilters={[
            { column: 'FirstName', operator: '_eq', value: 'John Doe' },
          ]}
        />,
      ),
    ).not.toThrow();

    expect(screen.getByTestId('filters-filter-rows')).toBeInTheDocument();
  });
});

describe('FilterRows column options', () => {
  it('does not mutate the columns array it is given', () => {
    const tableColumns = [
      { name: 'Zeta', dataType: 'string', consoleDataType: 'string' },
      { name: 'Alpha', dataType: 'string', consoleDataType: 'string' },
      { name: 'Mike', dataType: 'string', consoleDataType: 'string' },
    ] as unknown as TableColumn[];

    render(<Harness tableColumns={tableColumns} />);

    // The dropdown lists the columns alphabetically, but the caller's array
    // must keep its introspection (table) order: it is the array held in the
    // introspection query cache and reused for the browse-rows selection.
    expect(tableColumns.map((column) => column.name)).toEqual([
      'Zeta',
      'Alpha',
      'Mike',
    ]);
  });
});
