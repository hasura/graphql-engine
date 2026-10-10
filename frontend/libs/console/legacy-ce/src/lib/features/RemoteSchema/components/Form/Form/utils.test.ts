import type { RemoteSchema } from '@hasura/shared/types';
import { Schema } from './schema';
import { createRemoteSchemaFormValues, transformFormData } from './utils';

const formValues = (overrides: Partial<Schema> = {}): Schema => ({
  name: 'remote-schema',
  url: { type: 'from_url', value: 'https://example.com/graphql' },
  headers: [{ name: 'x-runtime', value: 'runtime', type: 'value' }],
  use_introspection_headers: false,
  introspection_headers: [],
  forward_client_headers: false,
  timeout_seconds: '',
  comment: '',
  customization: {
    root_fields_namespace: '',
    type_prefix: '',
    type_suffix: '',
    query_root: { parent_type: '', prefix: '', suffix: '' },
    mutation_root: { parent_type: '', prefix: '', suffix: '' },
  },
  ...overrides,
});

describe('transformFormData introspection headers', () => {
  it('omits introspection_headers when separate headers are disabled', () => {
    const { definition } = transformFormData(formValues());

    expect(definition).not.toHaveProperty('introspection_headers');
  });

  it('preserves an explicitly empty introspection_headers list', () => {
    const { definition } = transformFormData(
      formValues({ use_introspection_headers: true }),
    );

    expect(definition.introspection_headers).toEqual([]);
  });

  it('maps separate introspection headers without changing runtime headers', () => {
    const { definition } = transformFormData(
      formValues({
        use_introspection_headers: true,
        introspection_headers: [
          { name: 'x-introspection', value: 'SECRET', type: 'env' },
        ],
      }),
    );

    expect(definition.headers).toEqual([
      { name: 'x-runtime', value: 'runtime' },
    ]);
    expect(definition.introspection_headers).toEqual([
      { name: 'x-introspection', value_from_env: 'SECRET' },
    ]);
  });
});

describe('createRemoteSchemaFormValues introspection headers', () => {
  const remoteSchema = (
    definition: RemoteSchema['definition'],
  ): RemoteSchema => ({
    name: 'remote-schema',
    comment: '',
    definition,
  });

  it('treats an absent introspection_headers property as disabled', () => {
    const values = createRemoteSchemaFormValues(
      remoteSchema({
        url: 'https://example.com/graphql',
        headers: [{ name: 'x-runtime', value: 'runtime' }],
      }),
    );

    expect(values.use_introspection_headers).toBe(false);
    expect(values.introspection_headers).toEqual([]);
    // runtime headers are still hydrated for editing
    expect(values.headers).toEqual([
      { name: 'x-runtime', value: 'runtime', type: 'value' },
    ]);
  });

  it('treats an explicit empty introspection_headers list as enabled', () => {
    const values = createRemoteSchemaFormValues(
      remoteSchema({
        url: 'https://example.com/graphql',
        introspection_headers: [],
      }),
    );

    expect(values.use_introspection_headers).toBe(true);
    expect(values.introspection_headers).toEqual([]);
  });

  it('hydrates non-empty introspection headers as enabled', () => {
    const values = createRemoteSchemaFormValues(
      remoteSchema({
        url: 'https://example.com/graphql',
        introspection_headers: [
          { name: 'x-introspection', value_from_env: 'SECRET' },
        ],
      }),
    );

    expect(values.use_introspection_headers).toBe(true);
    expect(values.introspection_headers).toEqual([
      { name: 'x-introspection', value: 'SECRET', type: 'env' },
    ]);
  });
});

describe('transformFormData url and connection settings', () => {
  it('serializes a manual url definition', () => {
    const { definition } = transformFormData(
      formValues({
        url: { type: 'from_url', value: 'https://a.test/graphql' },
      }),
    );

    expect(definition).toMatchObject({ url: 'https://a.test/graphql' });
    expect(definition).not.toHaveProperty('url_from_env');
  });

  it('serializes an env url definition', () => {
    const { definition } = transformFormData(
      formValues({ url: { type: 'from_env', value: 'REMOTE_SCHEMA_URL' } }),
    );

    expect(definition).toMatchObject({ url_from_env: 'REMOTE_SCHEMA_URL' });
    expect(definition).not.toHaveProperty('url');
  });

  it('defaults an empty timeout to 60 seconds and forwards the flag', () => {
    // the shared formValues() helper leaves timeout_seconds empty
    const { definition } = transformFormData(
      formValues({ forward_client_headers: true }),
    );

    expect(definition.timeout_seconds).toBe(60);
    expect(definition.forward_client_headers).toBe(true);
  });

  it('preserves an explicit timeout value', () => {
    const { definition } = transformFormData(
      formValues({ timeout_seconds: 120 }),
    );

    expect(definition.timeout_seconds).toBe(120);
  });

  it('carries the name and comment through', () => {
    const { name, comment } = transformFormData(
      formValues({ name: 'my-schema', comment: 'notes' }),
    );

    expect(name).toBe('my-schema');
    expect(comment).toBe('notes');
  });
});

describe('transformFormData customization', () => {
  const customization = (
    overrides: Partial<Schema['customization']> = {},
  ): Schema['customization'] => ({
    root_fields_namespace: '',
    type_prefix: '',
    type_suffix: '',
    query_root: { parent_type: '', prefix: '', suffix: '' },
    mutation_root: { parent_type: '', prefix: '', suffix: '' },
    ...overrides,
  });

  it('emits an empty customization when nothing is set', () => {
    const { definition } = transformFormData(formValues());

    expect(definition.customization).toEqual({});
  });

  it('maps the root fields namespace', () => {
    const { definition } = transformFormData(
      formValues({
        customization: customization({ root_fields_namespace: 'namespace_' }),
      }),
    );

    expect(definition.customization?.root_fields_namespace).toBe('namespace_');
  });

  it('keeps only the non-empty type prefix/suffix in type_names', () => {
    const { definition } = transformFormData(
      formValues({ customization: customization({ type_prefix: 'p_' }) }),
    );

    expect(definition.customization?.type_names).toEqual({ prefix: 'p_' });
  });

  it('adds the query root field customization', () => {
    const { definition } = transformFormData(
      formValues({
        customization: customization({
          query_root: { parent_type: 'Query', prefix: 'q_', suffix: '' },
        }),
      }),
    );

    expect(definition.customization?.field_names).toEqual([
      { parent_type: 'Query', prefix: 'q_', suffix: undefined },
    ]);
  });

  it('adds the mutation root field customization', () => {
    const { definition } = transformFormData(
      formValues({
        customization: customization({
          mutation_root: { parent_type: 'Mutation', prefix: '', suffix: '_m' },
        }),
      }),
    );

    expect(definition.customization?.field_names).toEqual([
      { parent_type: 'Mutation', prefix: undefined, suffix: '_m' },
    ]);
  });

  // Regression: field_names is a list keyed by parent_type, so setting both the
  // query and mutation root customizations must keep both entries. Previously
  // the mutation assignment overwrote the query one.
  it('keeps both query and mutation root customizations', () => {
    const { definition } = transformFormData(
      formValues({
        customization: customization({
          query_root: { parent_type: 'Query', prefix: 'q_', suffix: '' },
          mutation_root: { parent_type: 'Mutation', prefix: 'm_', suffix: '' },
        }),
      }),
    );

    expect(definition.customization?.field_names).toEqual([
      { parent_type: 'Query', prefix: 'q_', suffix: undefined },
      { parent_type: 'Mutation', prefix: 'm_', suffix: undefined },
    ]);
  });
});

describe('createRemoteSchemaFormValues url and connection settings', () => {
  const remoteSchema = (
    definition: RemoteSchema['definition'],
    extra: Partial<Omit<RemoteSchema, 'definition'>> = {},
  ): RemoteSchema => ({
    name: 'remote-schema',
    comment: '',
    definition,
    ...extra,
  });

  it('hydrates an env url definition', () => {
    const values = createRemoteSchemaFormValues(
      remoteSchema({ url_from_env: 'REMOTE_SCHEMA_URL' }),
    );

    expect(values.url).toEqual({
      type: 'from_env',
      value: 'REMOTE_SCHEMA_URL',
    });
  });

  it('hydrates a manual url definition', () => {
    const values = createRemoteSchemaFormValues(
      remoteSchema({ url: 'https://a.test/graphql' }),
    );

    expect(values.url).toEqual({
      type: 'from_url',
      value: 'https://a.test/graphql',
    });
  });

  it('falls back to sensible defaults with no defaultValues', () => {
    const values = createRemoteSchemaFormValues();

    expect(values.url).toEqual({ type: 'from_url', value: '' });
    expect(values.name).toBe('');
    expect(values.comment).toBe('');
    expect(values.forward_client_headers).toBe(false);
    expect(values.timeout_seconds).toBe(60);
    expect(values.headers).toEqual([]);
  });

  it('round-trips name, comment, forward flag and timeout', () => {
    const values = createRemoteSchemaFormValues(
      remoteSchema(
        {
          url: 'https://a.test/graphql',
          forward_client_headers: true,
          timeout_seconds: 90,
        },
        { name: 'edited', comment: 'hello' },
      ),
    );

    expect(values.name).toBe('edited');
    expect(values.comment).toBe('hello');
    expect(values.forward_client_headers).toBe(true);
    expect(values.timeout_seconds).toBe(90);
  });
});

describe('customization round-trip', () => {
  const remoteSchema = (
    definition: RemoteSchema['definition'],
  ): RemoteSchema => ({
    name: 'remote-schema',
    comment: '',
    definition,
  });

  const existingCustomization = {
    root_fields_namespace: 'namespace_',
    type_names: {
      prefix: 'prefix_',
      suffix: '_suffix',
      mapping: { Country: 'country_name' },
    },
    field_names: [
      {
        parent_type: 'Continent',
        prefix: 'prefix_',
        suffix: '_suffix',
        mapping: { code: 'country_code' },
      },
      { parent_type: 'query_root', prefix: 'q_', mapping: { a: 'b' } },
    ],
  };

  it('loads the editable customization fields of an existing schema', () => {
    const { customization } = createRemoteSchemaFormValues(
      remoteSchema({
        url: 'https://a.test/graphql',
        customization: existingCustomization,
      }),
    );

    expect(customization.root_fields_namespace).toBe('namespace_');
    expect(customization.type_prefix).toBe('prefix_');
    expect(customization.type_suffix).toBe('_suffix');
  });

  it('saving an unchanged form keeps the whole customization', () => {
    const values = createRemoteSchemaFormValues(
      remoteSchema({
        url: 'https://a.test/graphql',
        customization: existingCustomization,
      }),
    );

    const { definition } = transformFormData(values, existingCustomization);

    expect(definition.customization).toEqual(existingCustomization);
  });

  it('applies form edits on top of mappings it cannot edit', () => {
    const { definition } = transformFormData(
      formValues({
        customization: {
          root_fields_namespace: 'ns_',
          type_prefix: 'p_',
          type_suffix: '',
          query_root: { parent_type: 'query_root', prefix: 'qq_', suffix: '' },
          mutation_root: { parent_type: '', prefix: '', suffix: '' },
        },
      }),
      existingCustomization,
    );

    expect(definition.customization).toEqual({
      root_fields_namespace: 'ns_',
      // the loaded suffix was cleared in the form
      type_names: {
        prefix: 'p_',
        mapping: { Country: 'country_name' },
      },
      field_names: [
        existingCustomization.field_names[0],
        { parent_type: 'query_root', prefix: 'qq_', mapping: { a: 'b' } },
      ],
    });
  });
});
