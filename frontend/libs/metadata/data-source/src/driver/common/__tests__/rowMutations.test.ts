import {
  buildSchema,
  GraphQLSchema,
  introspectionFromSchema,
  parse,
  validate as validateQuery,
} from 'graphql';
import { MetadataTable, Source } from '@hasura/shared/types';
import { deleteRowsCurry, insertRowsCurry, updateRowsCurry } from '../modify';
import { ValidateInputRowValuesFunction } from '../../types';

const validate: ValidateInputRowValuesFunction = ({ values }) => values;
const getDefaultQueryRoot = () => 'album';

const endpoints = {
  metadata: 'http://localhost:8080/v1/metadata',
  graphQLUrl: 'http://localhost:8080/v1/graphql',
} as any;

const schema = buildSchema(`
  input Int_comparison_exp { _eq: Int }
  input album_bool_exp { id: Int_comparison_exp }
  input album_insert_input { id: Int title: String }
  input album_set_input { title: String }

  type album { id: Int! title: String }
  extend input album_bool_exp { _or: [album_bool_exp!] }
  type album_mutation_response { affected_rows: Int! returning: [album!]! }

  type query_root { album: [album!]! }
  type mutation_root {
    insert_album(objects: [album_insert_input!]!): album_mutation_response
    update_album(_set: album_set_input, where: album_bool_exp!): album_mutation_response
    delete_album(where: album_bool_exp!): album_mutation_response
  }
  schema { query: query_root mutation: mutation_root }
`);

// `graphql-default` naming convention, a namespace and a custom column name
const customizedSchema = buildSchema(`
  input IntComparisonExp { _eq: Int }
  input AlbumBoolExp { id: IntComparisonExp }
  input AlbumInsertInput { id: Int name: String }

  type Album { id: Int! name: String }
  type AlbumMutationResponse { affectedRows: Int! returning: [Album!]! }

  type music_mutation_frontend {
    insertAlbum(objects: [AlbumInsertInput!]!): AlbumMutationResponse
  }
  type query_root { noop: Int }
  type mutation_root { music: music_mutation_frontend }
  schema { query: query_root mutation: mutation_root }
`);

const source = { name: 'chinook', kind: 'postgres' } as Source;

// the schema is cached per resource version, so each test gets its own
let resourceVersion = 0;

const mockServer = ({
  graphqlSchema = schema,
  configuration,
  customization,
  mutationData,
}: {
  graphqlSchema?: GraphQLSchema;
  configuration?: MetadataTable['configuration'];
  customization?: Source['customization'];
  mutationData: unknown;
}) => {
  resourceVersion += 1;
  const mutations: { query: string; variables: Record<string, unknown> }[] = [];

  const fetchJson = vi.fn(async (url: string, options: { body: string }) => {
    const body = JSON.parse(options.body);
    if (url === endpoints.metadata)
      return {
        resource_version: resourceVersion,
        metadata: {
          version: 3,
          sources: [
            {
              ...source,
              customization,
              tables: [{ table: ['album'], configuration }],
            },
          ],
        },
      };

    if (body.query.includes('__schema'))
      return { data: introspectionFromSchema(graphqlSchema) };

    mutations.push(body);
    return { data: mutationData };
  });

  return { fetchJson: fetchJson as any, mutations };
};

const expectValid = (graphqlSchema: GraphQLSchema, query: string) =>
  expect(validateQuery(graphqlSchema, parse(query))).toEqual([]);

describe('row mutations', () => {
  it('inserts rows through GraphQL', async () => {
    const { fetchJson, mutations } = mockServer({
      mutationData: { insert_album: { affected_rows: 2 } },
    });

    const affectedRows = await insertRowsCurry(
      validate,
      getDefaultQueryRoot,
    )({
      endpoints,
      fetchJson,
      args: {
        source,
        table: ['album'],
        objects: [{ title: 'a' }, { title: 'b' }],
        columns: [],
      },
    });

    expect(affectedRows).toBe(2);
    expectValid(schema, mutations[0].query);
    expect(mutations[0].variables).toEqual({
      objects: [{ title: 'a' }, { title: 'b' }],
    });
  });

  it('honours customizations and reads the namespaced result', async () => {
    const { fetchJson, mutations } = mockServer({
      graphqlSchema: customizedSchema,
      configuration: { column_config: { title: { custom_name: 'name' } } },
      customization: { root_fields: { namespace: 'music' } },
      mutationData: { music: { insertAlbum: { affectedRows: 1 } } },
    });

    const affectedRows = await insertRowsCurry(
      validate,
      getDefaultQueryRoot,
    )({
      endpoints,
      fetchJson,
      args: {
        source,
        table: ['album'],
        objects: [{ title: 'a' }],
        columns: [],
      },
    });

    expect(affectedRows).toBe(1);
    expectValid(customizedSchema, mutations[0].query);
    expect(mutations[0].variables).toEqual({ objects: [{ name: 'a' }] });
  });

  it('updates rows matching the filter', async () => {
    const { fetchJson, mutations } = mockServer({
      mutationData: { update_album: { affected_rows: 1 } },
    });

    const affectedRows = await updateRowsCurry(
      validate,
      getDefaultQueryRoot,
    )({
      endpoints,
      fetchJson,
      args: {
        source,
        table: ['album'],
        set: { title: 'c' },
        where: { id: 1 },
        columns: [{ name: 'id', dataType: 'int', consoleDataType: 'integer' }],
      },
    });

    expect(affectedRows).toBe(1);
    expectValid(schema, mutations[0].query);
    expect(mutations[0].variables).toEqual({
      _set: { title: 'c' },
      where: { id: { _eq: 1 } },
    });
  });

  it('deletes rows matching the filter', async () => {
    const { fetchJson, mutations } = mockServer({
      mutationData: { delete_album: { affected_rows: 3 } },
    });

    const affectedRows = await deleteRowsCurry(getDefaultQueryRoot)({
      endpoints,
      fetchJson,
      args: {
        source,
        table: ['album'],
        where: { id: 1 },
        columns: [{ name: 'id', dataType: 'int', consoleDataType: 'integer' }],
      },
    });

    expect(affectedRows).toBe(3);
    expectValid(schema, mutations[0].query);
    expect(mutations[0].variables).toEqual({ where: { id: { _eq: 1 } } });
  });

  it('deletes the selected rows matching any of the filters', async () => {
    const { fetchJson, mutations } = mockServer({
      mutationData: { delete_album: { affected_rows: 2 } },
    });

    const affectedRows = await deleteRowsCurry(getDefaultQueryRoot)({
      endpoints,
      fetchJson,
      args: {
        source,
        table: ['album'],
        where: { _or: [{ id: { _eq: 1 } }, { id: { _eq: 2 } }] },
        columns: [{ name: 'id', dataType: 'int', consoleDataType: 'integer' }],
      },
    });

    expect(affectedRows).toBe(2);
    expectValid(schema, mutations[0].query);
    expect(mutations[0].variables).toEqual({
      where: { _or: [{ id: { _eq: 1 } }, { id: { _eq: 2 } }] },
    });
  });

  it('refuses to update or delete without a filter', async () => {
    const { fetchJson, mutations } = mockServer({ mutationData: {} });

    // `id` isn't a known column, so the filter ends up empty
    await expect(
      deleteRowsCurry(getDefaultQueryRoot)({
        endpoints,
        fetchJson,
        args: { source, table: ['album'], where: { id: 1 }, columns: [] },
      }),
    ).rejects.toThrow('Refusing to delete rows without a filter');
    expect(fetchJson).not.toHaveBeenCalled();
    expect(mutations).toEqual([]);
  });

  it.each([
    ['a key value', { id: undefined }],
    ['a selected row key', { _or: [{ id: { _eq: undefined } }] }],
  ])(
    'refuses to delete when %s is missing instead of matching every row',
    async (_, where) => {
      const { fetchJson, mutations } = mockServer({ mutationData: {} });

      await expect(
        deleteRowsCurry(getDefaultQueryRoot)({
          endpoints,
          fetchJson,
          args: {
            source,
            table: ['album'],
            where,
            columns: [
              { name: 'id', dataType: 'int', consoleDataType: 'integer' },
            ],
          },
        }),
      ).rejects.toThrow('Refusing to delete rows without a filter');
      expect(mutations).toEqual([]);
    },
  );

  it('reports a table whose mutation is not exposed', async () => {
    const { fetchJson } = mockServer({ mutationData: {} });

    await expect(
      insertRowsCurry(
        validate,
        () => 'untracked',
      )({
        endpoints,
        fetchJson,
        args: { source, table: ['album'], objects: [{}], columns: [] },
      }),
    ).rejects.toThrow(
      'insertRows: the GraphQL schema exposes no insert mutation for untracked',
    );
  });
});
