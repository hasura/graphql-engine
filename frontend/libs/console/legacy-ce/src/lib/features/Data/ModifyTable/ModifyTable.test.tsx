import { render, screen } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import type { MetadataTable, Source } from '@hasura/shared/types';
import { ModifyTable } from './ModifyTable';

// Stub all section child components so this test isolates ModifyTable's
// per-driver gating wiring (each child has its own dedicated tests).
vi.mock('./components', () => {
  const Stub = (name: string) => () => <div data-testid={`section-${name}`} />;
  return {
    TableColumns: Stub('columns'),
    TableComments: Stub('comments'),
    TableRootFields: Stub('rootfields'),
    ForeignKeys: Stub('fk'),
    ComputedFields: Stub('computed'),
    SetAsEnum: Stub('enum'),
    CheckConstraints: Stub('check'),
    PrimaryKey: Stub('pk'),
    UniqueKeys: Stub('unique'),
    Indexes: Stub('indexes'),
    Triggers: Stub('triggers'),
  };
});
vi.mock('./components/ApolloFederation', () => ({
  ApolloFederation: () => null,
}));
vi.mock('./components/ViewDefinition', () => ({ default: () => null }));
vi.mock('./hooks/useApolloFederationSupportedDrivers', () => ({
  useApolloFederationSupportedDrivers: () => [],
}));
vi.mock('@hasura/shared/context', () => ({
  useAppContext: () => ({ readOnlyMode: false }),
}));
vi.mock('@hasura/metadata/helpers', () => ({
  isPostgresFlavour: (k: string) => k === 'postgres',
  MetadataSelectors: { getSupportsForeignKeys: () => pgSupported },
}));

let pgSupported = true;

const pgMethods = {
  introspection: {
    getCheckConstraints: () => undefined,
    getPrimaryKey: () => undefined,
    getUniqueKeys: () => undefined,
    getTableIndexes: () => undefined,
    getTableTriggers: () => undefined,
  },
  modify: {
    alterTableComment: () => undefined,
    createCheckConstraint: () => undefined,
    createPrimaryKey: () => undefined,
    alterPrimaryKey: () => undefined,
    createUniqueKey: () => undefined,
  },
  check: { isFeatureSupported: () => true },
};
const gdcMethods = {
  introspection: {},
  modify: {},
  check: { isFeatureSupported: () => false },
};

vi.mock('@hasura/metadata/data-source', () => ({
  getDatabaseMethods: () => (pgSupported ? pgMethods : gdcMethods),
}));

const table = { table: { name: 'orders', schema: 'public' } } as MetadataTable;
const render_ = (kind: string) =>
  render(
    <Theme>
      <ModifyTable
        source={{ name: 'default', kind } as Source}
        table={table}
        isView={false}
      />
    </Theme>,
  );

describe('ModifyTable section gating', () => {
  it('renders all native-PG sections for a supported (postgres) driver', () => {
    pgSupported = true;
    render_('postgres');
    ['fk', 'pk', 'unique', 'check', 'indexes', 'triggers', 'enum'].forEach(
      (s) => expect(screen.getByTestId(`section-${s}`)).toBeInTheDocument(),
    );
  });

  it('hides native-PG constraint/index/trigger sections for an unsupported (gdc) driver', () => {
    pgSupported = false;
    render_('sqlite');
    ['pk', 'unique', 'check', 'indexes', 'triggers'].forEach((s) =>
      expect(screen.queryByTestId(`section-${s}`)).not.toBeInTheDocument(),
    );
  });
});
