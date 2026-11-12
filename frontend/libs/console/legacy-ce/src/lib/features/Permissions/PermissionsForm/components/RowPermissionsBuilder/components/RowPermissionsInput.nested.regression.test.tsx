import { render, screen } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { ComputedField } from '@hasura/shared/types';
import { RowPermissionsInput } from './RowPermissionsInput';
import { Permissions, Tables } from './types';

// Radix Select doesn't render its options in jsdom; a native <select> does.
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

// jsdom lacks ResizeObserver, which the _exists table picker depends on.
beforeAll(() => {
  globalThis.ResizeObserver =
    globalThis.ResizeObserver ??
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
});

const column = (name: string, scalarType = 'String') => ({
  name,
  dataType: scalarType.toLowerCase(),
  graphQLProperties: { name, scalarType },
});

// Regression fixtures modelled on issue #10907: a games table whose existing
// select filter references an array relationship, an object relationship, a
// scalar computed field and an `_exists` over another table.
const tables: Tables = [
  {
    table: { schema: 'public', name: 'games' },
    dataSource: { name: 'default', kind: 'postgres' },
    columns: [
      column('isEnabled', 'Boolean'),
      column('isTest', 'Boolean'),
      column('canSitePlayerAccess', 'Boolean'),
    ],
    computedFields: [{ name: 'full_name' } as ComputedField],
    relationships: [
      {
        name: 'site_bannedgames',
        relationshipType: 'Array',
        type: 'localRelationship',
        fromSource: 'default',
        fromTable: { schema: 'public', name: 'games' },
        definition: {
          mapping: { id: 'game_id' },
          toTable: { schema: 'public', name: 'site_bannedgames' },
        },
      } as Tables[number]['relationships'][number],
      {
        name: 'siteProvider',
        relationshipType: 'Object',
        type: 'localRelationship',
        fromSource: 'default',
        fromTable: { schema: 'public', name: 'games' },
        definition: {
          mapping: { provider_id: 'id' },
          toTable: { schema: 'public', name: 'providers' },
        },
      } as Tables[number]['relationships'][number],
    ],
  },
  {
    table: { schema: 'public', name: 'site_bannedgames' },
    dataSource: { name: 'default', kind: 'postgres' },
    columns: [column('site_id'), column('game_id')],
    computedFields: [],
    relationships: [],
  },
  {
    table: { schema: 'public', name: 'providers' },
    dataSource: { name: 'default', kind: 'postgres' },
    columns: [column('isActive', 'Boolean'), column('id')],
    computedFields: [],
    relationships: [],
  },
  {
    table: { schema: 'public', name: 'user_variables' },
    dataSource: { name: 'default', kind: 'postgres' },
    columns: [column('name'), column('user_id')],
    computedFields: [],
    relationships: [],
  },
];

const setup = (permissions: Permissions) =>
  render(
    <Theme>
      <RowPermissionsInput
        permissions={permissions}
        table={{ schema: 'public', name: 'games' }}
        source="default"
        tables={tables}
        logicalModel={undefined}
        logicalModels={[]}
        comparators={{}}
        onPermissionsChange={vi.fn()}
      />
    </Theme>,
  );

describe('existing nested filters render in the builder (issue #10907)', () => {
  it('renders a condition nested under an array relationship inside _not', () => {
    setup({
      _not: { site_bannedgames: { site_id: { _eq: 'X-Hasura-Site-Id' } } },
    });

    expect(
      screen.getByTestId('_not.site_bannedgames.site_id._eq-value-input'),
    ).toBeInTheDocument();
  });

  it('renders a condition nested under an object relationship', () => {
    setup({ siteProvider: { isActive: { _eq: true } } });

    expect(
      screen.getByTestId('siteProvider.isActive._eq-value-input'),
    ).toBeInTheDocument();
  });

  it('renders a scalar computed field comparison', () => {
    setup({ full_name: { _eq: 'X-Hasura-User-Id' } });

    expect(screen.getByTestId('full_name._eq-value-input')).toBeInTheDocument();
  });

  it('renders an _exists condition over another table', () => {
    setup({
      _exists: {
        _table: { schema: 'public', name: 'user_variables' },
        _where: { name: { _eq: 'tester' } },
      },
    });

    expect(
      screen.getByTestId('_exists._where.name._eq-value-input'),
    ).toBeInTheDocument();
  });

  it('renders the full issue #10907 filter with every operator kind at once', () => {
    setup({
      _and: [
        {
          _not: { site_bannedgames: { site_id: { _eq: 'X-Hasura-Site-Id' } } },
        },
        { isEnabled: { _eq: true } },
        { siteProvider: { isActive: { _eq: true } } },
        { full_name: { _eq: 'X-Hasura-User-Id' } },
        {
          _exists: {
            _table: { schema: 'public', name: 'user_variables' },
            _where: { name: { _eq: 'tester' } },
          },
        },
      ],
    });

    expect(
      screen.getByTestId(
        '_and.0._not.site_bannedgames.site_id._eq-value-input',
      ),
    ).toBeInTheDocument();
    expect(
      screen.getByTestId('_and.2.siteProvider.isActive._eq-value-input'),
    ).toBeInTheDocument();
    expect(
      screen.getByTestId('_and.3.full_name._eq-value-input'),
    ).toBeInTheDocument();
    expect(
      screen.getByTestId('_and.4._exists._where.name._eq-value-input'),
    ).toBeInTheDocument();
  });
});
