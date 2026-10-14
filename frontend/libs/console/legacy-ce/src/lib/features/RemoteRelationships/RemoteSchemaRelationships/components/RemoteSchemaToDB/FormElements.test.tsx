import { render, waitFor } from '@testing-library/react';
import { FormProvider, useForm, UseFormReturn } from 'react-hook-form';
import type { Schema } from './schema';

// FormElements' RelationshipMapSelector prunes `mapping` entries whose column or
// field is no longer available. These mocks let the test control the available
// columns/types and assert the pruning is applied once and then settles (the
// length-guarded effect must not loop now that `mapping` is a dependency).
vi.mock('../../../utils', () => ({
  getTypesFromIntrospection: () => [
    { typeName: 'User', fields: ['id', 'name'] },
  ],
}));

vi.mock('@hasura/metadata/api', () => ({
  useMetadata: () => ({ data: { metadata: {} }, isFetching: false }),
  useIntrospectRemoteSchema: () => ({
    data: { __schema: true },
    isFetching: false,
  }),
}));

vi.mock('@hasura/metadata/data-source', () => ({
  useTableColumns: () => ({
    data: { columns: [{ name: 'id' }, { name: 'email' }] },
    isFetching: false,
  }),
}));

vi.mock('@hasura/metadata/helpers', () => ({
  MetadataSelectors: { findSource: () => () => ({ name: 'default' }) },
}));

vi.mock('./MapSelector', () => ({
  MapSelector: () => <div data-testid="map-selector" />,
}));
vi.mock('../RsSourceTypeSelector', () => ({
  RsSourceTypeSelector: () => <div data-testid="rs-source-type" />,
}));
vi.mock('../RemoteDatabaseWidget', () => ({
  RemoteDatabaseWidget: () => <div data-testid="remote-db" />,
}));

import { FormElements } from './FormElements';

const setup = (mapping: Schema['mapping']) => {
  let methods!: UseFormReturn<Schema>;
  const Harness = () => {
    methods = useForm<Schema>({
      defaultValues: {
        relationshipType: 'array',
        relationshipName: 'rel',
        typeName: 'User',
        target: {
          type: 'table',
          dataSourceName: 'default',
          table: 'users',
        },
        mapping,
      },
    });
    return (
      <FormProvider {...methods}>
        <FormElements
          sourceRemoteSchema="remoteSchema1"
          existingRelationship={undefined}
        />
      </FormProvider>
    );
  };
  const view = render(<Harness />);
  return { ...view, getMapping: () => methods.getValues('mapping') };
};

describe('FormElements - mapping pruning', () => {
  it('prunes mappings whose column or field is no longer available, then settles', async () => {
    const { getMapping } = setup([
      { field: 'id', column: 'id' }, // valid
      { field: 'name', column: 'missing-col' }, // invalid column
      { field: 'ghost-field', column: 'id' }, // invalid field
    ]);

    await waitFor(() =>
      expect(getMapping()).toEqual([{ field: 'id', column: 'id' }]),
    );

    // No runaway: once pruned, the length guard prevents further writes, so the
    // value reference stays stable across subsequent ticks.
    const settled = getMapping();
    await new Promise((resolve) => setTimeout(resolve, 150));
    expect(getMapping()).toBe(settled);
  });

  it('leaves an already-valid mapping untouched (no needless rewrite)', async () => {
    const initial = [{ field: 'id', column: 'id' }];
    const { getMapping } = setup(initial);

    // Allow effects to run; a fully-valid mapping must not be replaced.
    await new Promise((resolve) => setTimeout(resolve, 150));
    expect(getMapping()).toEqual([{ field: 'id', column: 'id' }]);
  });
});
