import { render, screen } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { useConsoleForm } from '@hasura/shared/ui';
import { z } from 'zod';
import type { TableColumn, Operator } from '@hasura/metadata/data-source';
import { FilterRows } from './FilterRows';
import { getFilterValueKind, toFilterValue } from './FilterRow';

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
  columnOperators,
  enumColumnValues,
}: {
  initialFilters?: { column: string; operator: string; value: string }[];
  tableColumns?: TableColumn[];
  columnOperators?: Record<string, Operator[]>;
  enumColumnValues?: Record<string, string[]>;
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
        columnOperators={columnOperators}
        enumColumnValues={enumColumnValues}
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

describe('FilterRows column operators', () => {
  const columnOperators: Record<string, Operator[]> = {
    FirstName: [
      { name: 'equals', value: '_eq' },
      { name: 'like', value: '_like' },
    ],
    ID: [
      { name: 'equals', value: '_eq' },
      { name: '>', value: '_gt' },
    ],
  };

  it('keeps an operator the column supports', async () => {
    render(
      <Harness
        columnOperators={columnOperators}
        initialFilters={[
          { column: 'FirstName', operator: '_like', value: 'J%' },
        ]}
      />,
    );
    expect(await screen.findByText('[_like] like')).toBeInTheDocument();
  });

  it('replaces an operator the column does not support', async () => {
    render(
      <Harness
        columnOperators={columnOperators}
        // a number column has no `_like`
        initialFilters={[{ column: 'ID', operator: '_like', value: '1' }]}
      />,
    );
    expect(await screen.findByText('[_eq] equals')).toBeInTheDocument();
    expect(screen.queryByText(/_like/)).not.toBeInTheDocument();
  });

  it('keeps any operator while the column operators are unknown', async () => {
    render(
      <Harness
        initialFilters={[{ column: 'ID', operator: '_neq', value: '1' }]}
      />,
    );
    expect(await screen.findByText('[_neq] _neq')).toBeInTheDocument();
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

describe('FilterRows value inputs', () => {
  beforeAll(() => {
    // Radix inputs rely on DOM APIs jsdom does not implement
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

  it.each([
    ['_in', 'list'],
    ['_nin', 'list'],
    ['_is_null', 'boolean'],
    ['_isNull', 'boolean'],
    ['_eq', 'text'],
    [undefined, 'text'],
  ])('%s takes a %s value', (operator, kind) => {
    expect(getFilterValueKind(operator)).toBe(kind);
  });

  const column = (
    consoleDataType: TableColumn['consoleDataType'],
    dataType = consoleDataType as string,
  ) => ({ name: 'c', dataType, consoleDataType }) as TableColumn;
  const operator = (value: string, valueType: string, isListValue = false) =>
    ({ name: value, value, valueType, isListValue }) as Operator;

  it.each([
    // the operand type of the operator wins
    [
      'jsonb `_contains`',
      operator('_contains', 'jsonb'),
      column('json'),
      'json',
    ],
    [
      'jsonb `_has_key`',
      operator('_has_key', 'String'),
      column('json'),
      'text',
    ],
    [
      'jsonb `_has_keys_any`',
      operator('_has_keys_any', 'String', true),
      column('json'),
      'list',
    ],
    ['boolean `_eq`', operator('_eq', 'Boolean'), column('boolean'), 'boolean'],
    [
      'timestamptz `_gt`',
      operator('_gt', 'timestamptz'),
      column('timestamp'),
      'datetime',
    ],
    // a Data Connector datetime is a `String`
    [
      'datetime `_eq`',
      operator('_eq', 'String'),
      column('timestamp'),
      'datetime',
    ],
    ['text `_like`', operator('_like', 'String'), column('text'), 'text'],
    // without operand types, the column type picks the input
    ['json `_eq`', undefined, column('json'), 'json'],
    ['boolean `_eq`', undefined, column('boolean'), 'boolean'],
    ['timestamp `_eq`', undefined, column('timestamp'), 'datetime'],
  ])('%s takes a %s value', (_, op, col, kind) => {
    expect(
      getFilterValueKind(op?.value ?? '_eq', { operator: op, column: col }),
    ).toBe(kind);
  });

  describe('toFilterValue', () => {
    const date = new Date('2026-10-22T10:48:04Z');

    it('parses a JSON document', () => {
      expect(toFilterValue('{"a": [1]}', 'json')).toEqual({ a: [1] });
      // invalid JSON is sent as-is for the server to report
      expect(toFilterValue('{a', 'json')).toBe('{a');
    });

    it('formats a date for the time zone of the column', () => {
      expect(
        toFilterValue(date, 'datetime', column('timestamp', 'timestamptz')),
      ).toBe('2026-10-22T10:48:04.000Z');
      // the tests run in UTC, the local time of a column without time zone
      expect(
        toFilterValue(
          date,
          'datetime',
          column('timestamp', 'timestamp without time zone'),
        ),
      ).toBe('2026-10-22T10:48:04');
    });

    it('keeps other values', () => {
      expect(toFilterValue('x', 'text')).toBe('x');
      expect(toFilterValue(['a'], 'list')).toEqual(['a']);
    });
  });

  it('checks a boolean column with a switch', async () => {
    render(
      <Theme>
        <Harness
          tableColumns={[column('boolean', 'boolean')].map((c) => ({
            ...c,
            name: 'active',
          }))}
          initialFilters={[
            { column: 'active', operator: '_eq', value: 'false' },
          ]}
        />
      </Theme>,
    );

    expect(await screen.findByRole('switch')).not.toBeChecked();
    expect(screen.getByText('false')).toBeInTheDocument();
  });

  it('picks the value of a timestamptz column with a date time picker', async () => {
    render(
      <Theme>
        <Harness
          tableColumns={[
            { ...column('timestamp', 'timestamptz'), name: 'created_at' },
          ]}
          initialFilters={[
            {
              column: 'created_at',
              operator: '_gt',
              value: '2026-10-22T10:48:04Z',
            },
          ]}
        />
      </Theme>,
    );

    // the date restored from the URL, in the picker's `Pp` format
    expect(await screen.findByDisplayValue(/10\/22\/2026/)).toBeInTheDocument();
    // and the ISO date it is queried with
    expect(screen.getByTestId('filters.0.value-iso')).toHaveTextContent(
      '2026-10-22T10:48:04.000Z',
    );
  });

  it('edits the values of `_in` as a list', async () => {
    render(
      <Theme>
        <Harness
          initialFilters={[
            { column: 'FirstName', operator: '_in', value: '["a","b c"]' },
          ]}
        />
      </Theme>,
    );

    const items = await screen.findAllByRole('textbox', {
      name: /filters\.0\.value item/,
    });
    expect(items.map((item) => (item as HTMLInputElement).value)).toEqual([
      'a',
      'b c',
    ]);
    expect(
      screen.getByRole('button', { name: 'Add item' }),
    ).toBeInTheDocument();
  });

  it('checks `_is_null` with a switch', async () => {
    render(
      <Theme>
        <Harness
          initialFilters={[
            { column: 'FirstName', operator: '_is_null', value: 'true' },
          ]}
        />
      </Theme>,
    );

    expect(await screen.findByRole('switch')).toBeChecked();
    expect(screen.getByText('is null')).toBeInTheDocument();
  });

  it('picks the value of an enum column from its values', async () => {
    render(
      <Theme>
        <Harness
          enumColumnValues={{ FirstName: ['admin', 'user'] }}
          initialFilters={[
            { column: 'FirstName', operator: '_eq', value: 'user' },
          ]}
        />
      </Theme>,
    );

    expect(await screen.findByText('user')).toBeInTheDocument();
    expect(
      screen.queryByRole('textbox', { name: /filters\.0\.value/ }),
    ).not.toBeInTheDocument();
  });

  it('picks the values of `_in` on an enum column', async () => {
    render(
      <Theme>
        <Harness
          enumColumnValues={{ FirstName: ['admin', 'user', 'guest'] }}
          initialFilters={[
            {
              column: 'FirstName',
              operator: '_in',
              value: '["admin","guest"]',
            },
          ]}
        />
      </Theme>,
    );

    expect(await screen.findByText('admin')).toBeInTheDocument();
    expect(screen.getByText('guest')).toBeInTheDocument();
    expect(screen.queryByText('user')).not.toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: 'Add item' }),
    ).not.toBeInTheDocument();
  });
});
