import { fireEvent, render, screen, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { Theme } from '@radix-ui/themes';
import { RowPermissionsInput } from './RowPermissionsInput';
import { toColumnValue } from './ValueInputType';
import { isColumnComparator } from './utils/helpers';
import { getSiblingKeys } from './Operator';
import { Permissions, Tables } from './types';

// Radix's select doesn't render its options in jsdom: a native one does
vi.mock('@hasura/shared/ui', async (importActual) => {
  const actual = await importActual<Record<string, unknown>>();
  return {
    ...actual,
    Select: ({
      value,
      onChange,
      options,
      'data-testid': testId,
    }: {
      value?: string;
      onChange?: (value: string) => void;
      options: { value: string; label: string }[];
      'data-testid'?: string;
    }) => (
      <select
        data-testid={testId}
        value={value}
        onChange={(event) => onChange?.(event.target.value)}
      >
        {options.map((option) => (
          <option key={option.value} value={option.value}>
            {option.label}
          </option>
        ))}
      </select>
    ),
  };
});

const column = (name: string, scalarType = 'String') => ({
  name,
  dataType: scalarType.toLowerCase(),
  graphQLProperties: { name, scalarType },
});

const usersTable = (source: string, columns: Tables[number]['columns']) => ({
  table: { schema: 'public', name: 'users' },
  dataSource: { name: source, kind: 'postgres' as const },
  columns,
  relationships: [],
  computedFields: [],
});

const setup = ({
  permissions,
  tables,
  source = 'default',
}: {
  permissions: Permissions;
  tables: Tables;
  source?: string;
}) => {
  const onPermissionsChange = vi.fn();
  render(
    <Theme>
      <RowPermissionsInput
        permissions={permissions}
        table={{ schema: 'public', name: 'users' }}
        source={source}
        tables={tables}
        logicalModel={undefined}
        logicalModels={[]}
        comparators={{}}
        onPermissionsChange={onPermissionsChange}
      />
    </Theme>,
  );
  return { onPermissionsChange };
};

const selectedKeys = () =>
  Array.from(document.querySelectorAll('[class*="singleValue"]')).map(
    (element) => element.textContent,
  );

describe('boolean values', () => {
  it('are stored as booleans', () => {
    const { onPermissionsChange } = setup({
      permissions: { name: { _is_null: false } },
      tables: [usersTable('default', [column('name')])],
    });

    fireEvent.change(screen.getByTestId('name._is_null-value-input'), {
      target: { value: 'true' },
    });

    expect(onPermissionsChange).toHaveBeenLastCalledWith({
      name: { _is_null: true },
    });
  });
});

describe('column comparators', () => {
  it.each(['_ceq', '_cne', '_cgt', '_clt', '_cgte', '_clte'])(
    'recognizes %s, as the server names it',
    (comparator) => {
      expect(isColumnComparator(comparator)).toBe(true);
    },
  );

  it('shows the column picker for _cgte', () => {
    setup({
      permissions: { age: { _cgte: ['min_age'] } },
      tables: [usersTable('default', [column('age'), column('min_age')])],
    });

    expect(
      screen.getByTestId('age._cgte-column-comparator-entry'),
    ).toHaveTextContent('min_age');
  });

  it('searches and picks a column', async () => {
    const { onPermissionsChange } = setup({
      permissions: { age: { _ceq: [] } },
      tables: [
        usersTable('default', [
          column('age'),
          column('min_age'),
          column('max_age'),
        ]),
      ],
    });

    await userEvent.type(
      screen.getByLabelText('age._ceq-column-comparator-entry'),
      'max',
    );

    // the search leaves only the matching column
    expect(
      screen.queryByTestId('age._ceq-column-comparator-entry-min_age'),
    ).not.toBeInTheDocument();
    await userEvent.click(
      screen.getByTestId('age._ceq-column-comparator-entry-max_age'),
    );

    expect(onPermissionsChange).toHaveBeenLastCalledWith({
      age: { _ceq: ['max_age'] },
    });
  });

  it('picks a column of the root table', async () => {
    const { onPermissionsChange } = setup({
      permissions: { age: { _ceq: ['$', ''] } },
      tables: [usersTable('default', [column('age'), column('min_age')])],
    });

    await userEvent.click(
      screen.getByLabelText('age._ceq-root-column-comparator-entry'),
    );
    await userEvent.click(
      screen.getByTestId('age._ceq-root-column-comparator-entry-min_age'),
    );

    expect(onPermissionsChange).toHaveBeenLastCalledWith({
      age: { _ceq: ['$', 'min_age'] },
    });
  });
});

describe('number values', () => {
  it('can be typed with decimals', () => {
    const { onPermissionsChange } = setup({
      permissions: { price: { _eq: '' } },
      tables: [usersTable('default', [column('price', 'Float')])],
    });
    const input = screen.getByTestId('price._eq-value-input');

    fireEvent.change(input, { target: { value: '1.' } });
    expect(input).toHaveValue('1.');

    fireEvent.change(input, { target: { value: '1.5' } });
    expect(input).toHaveValue('1.5');
    expect(onPermissionsChange).toHaveBeenLastCalledWith({
      price: { _eq: 1.5 },
    });
  });

  it.each([
    ['42', 'Int', 42],
    ['1.5', 'Int', '1.5'],
    ['-1.5e3', 'Float', -1500],
    ['1.', 'Float', '1.'],
    [' ', 'Float', ' '],
    ['X-Hasura-User-Id', 'Int', 'X-Hasura-User-Id'],
    ['42', 'String', '42'],
  ])('stores %j of an %s column as %j', (text, scalarType, expected) => {
    expect(toColumnValue(text, scalarType)).toEqual(expected);
  });
});

describe('unexpected values from the JSON editor', () => {
  it.each([
    ['a number for a column comparator', { age: { _ceq: 1 } }],
    ['an object for a column comparator', { age: { _ceq: {} } }],
    ['null for _exists', { _exists: null }],
  ])('render with %s', (_case, permissions) => {
    setup({
      permissions,
      tables: [usersTable('default', [column('age')])],
    });

    expect(screen.getByTestId('RootInputReady')).toBeInTheDocument();
  });
});

describe('getSiblingKeys', () => {
  const permissions = {
    name: { _eq: 'a' },
    _and: [{ age: { _eq: 1 }, id: { _eq: 2 } }, {}],
  };

  it('lists the other keys of the object holding the key', () => {
    expect(getSiblingKeys(permissions, ['name'], 'name')).toEqual(['_and']);
    expect(getSiblingKeys(permissions, ['_and', '0', 'age'], 'age')).toEqual([
      'id',
    ]);
  });

  it('lists the keys of the object a key is added to', () => {
    expect(getSiblingKeys(permissions, [], '')).toEqual(['name', '_and']);
    expect(getSiblingKeys(permissions, ['_and', '1'], '')).toEqual([]);
  });
});

describe('arrays', () => {
  it('give each item its own path', () => {
    const consoleError = vi.spyOn(console, 'error').mockImplementation(() => {
      /* noop */
    });

    setup({
      permissions: { _and: [{}, { name: { _eq: 'a' } }, {}] },
      tables: [usersTable('default', [column('name')])],
    });

    expect(
      screen.getByTestId('_and.1.name._eq-value-input'),
    ).toBeInTheDocument();
    // no items share a React key
    expect(
      consoleError.mock.calls.some(([message]) =>
        String(message).includes('same key'),
      ),
    ).toBe(false);
    consoleError.mockRestore();
  });
});

describe('sources with tables of the same name', () => {
  // the other source's table comes first and has no columns loaded
  const tables = [
    usersTable('other', []),
    usersTable('default', [column('name')]),
  ];

  it('uses the table of the permission source', () => {
    setup({ permissions: { name: { _eq: 'a' } }, tables });

    expect(selectedKeys()).toContain('name');
  });

  it('only offers the tables of the same source to _exists', () => {
    setup({
      permissions: { _exists: { _table: {}, _where: {} } },
      tables: [
        ...tables,
        {
          ...usersTable('other', []),
          table: { schema: 'public', name: 'orders' },
        },
      ],
    });

    const options = within(
      screen.getByTestId('_exists._table-value-input'),
    ).getAllByRole('option');
    expect(options.map((option) => option.textContent)).toEqual([
      'public.users',
    ]);
  });
});
