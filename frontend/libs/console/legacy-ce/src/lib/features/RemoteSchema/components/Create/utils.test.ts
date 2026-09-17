import { Schema } from './schema';
import { transformFormData } from './utils';

const formValues = (overrides: Partial<Schema> = {}): Schema => ({
  name: 'remote-schema',
  url: { type: 'from_url', value: 'https://example.com/graphql' },
  headers: [{ name: 'x-runtime', value: 'runtime', type: 'from_value' }],
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
      formValues({ use_introspection_headers: true })
    );

    expect(definition.introspection_headers).toEqual([]);
  });

  it('maps separate introspection headers without changing runtime headers', () => {
    const { definition } = transformFormData(
      formValues({
        use_introspection_headers: true,
        introspection_headers: [
          { name: 'x-introspection', value: 'SECRET', type: 'from_env' },
        ],
      })
    );

    expect(definition.headers).toEqual([
      { name: 'x-runtime', value: 'runtime' },
    ]);
    expect(definition.introspection_headers).toEqual([
      { name: 'x-introspection', value_from_env: 'SECRET' },
    ]);
  });
});
