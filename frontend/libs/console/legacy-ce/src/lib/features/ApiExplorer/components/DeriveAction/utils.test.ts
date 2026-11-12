import { buildSchema, OperationDefinitionNode, parse } from 'graphql';
import deriveAction, { getDataConnectorScalarReplacements } from './utils';
import { getTypesSdl } from '../../../../shared/utils/sdlUtils';

const schema = buildSchema(`
  enum user_role {
    admin
    editor
  }

  type user {
    id: Int!
    name: String
    email: String
    role: user_role
  }

  type user_mutation_response {
    affected_rows: Int!
    returning: [user!]!
  }

  input user_insert_input {
    name: String
    email: String
  }

  type query_root {
    user(limit: Int): [user!]!
  }

  type mutation_root {
    insert_user_one(object: user_insert_input!): user
    insert_user(objects: [user_insert_input!]!): user_mutation_response
  }

  schema {
    query: query_root
    mutation: mutation_root
  }
`);

const getOperation = (query: string) =>
  parse(query).definitions[0] as OperationDefinitionNode;

const outputFields = (result: ReturnType<typeof deriveAction>) => {
  const outputType = result.types.find((t) => t.kind === 'objects');
  return outputType?.kind === 'objects'
    ? outputType.definition.fields.map((f) => f.name)
    : [];
};

describe('deriveAction', () => {
  it('maps the selected scalar fields onto the output type', () => {
    const result = deriveAction(
      getOperation(`
        mutation insertUser($object: user_insert_input!) {
          insert_user_one(object: $object) {
            id
            name
          }
        }
      `),
      schema,
    );

    expect(result.action.name).toBe('insertUser');
    expect(result.action.output_type).toBe('InsertUserOutput');
    expect(outputFields(result)).toEqual(['id', 'name']);
  });

  it('returns the new types in the shape expected by getTypesSdl', () => {
    const result = deriveAction(
      getOperation(`
        mutation insertUser($object: user_insert_input!) {
          insert_user_one(object: $object) {
            id
            name
          }
        }
      `),
      schema,
    );

    const typesSdl = getTypesSdl(result.types);
    expect(typesSdl).toContain('input InsertUserUserInsertInput');
    expect(typesSdl).toContain('type InsertUserOutput');
    expect(typesSdl).toMatch(/id\s*:\s*Int!/);
    expect(typesSdl).toMatch(/email\s*:\s*String/);
  });

  it('keeps the list wrappers of the root field in the output type', () => {
    const result = deriveAction(
      getOperation(`
        query getUsers {
          user {
            id
          }
        }
      `),
      schema,
    );

    expect(result.action.output_type).toBe('[GetUsersOutput!]!');
  });

  it('ignores aliases on the root field and on the selected fields', () => {
    const result = deriveAction(
      getOperation(`
        query getUsers {
          something: user {
            alias1: id
            alias2: email
          }
        }
      `),
      schema,
    );

    expect(outputFields(result)).toEqual(['id', 'email']);
  });

  it('includes the fields selected through inline fragments', () => {
    const result = deriveAction(
      getOperation(`
        query getUsers {
          user {
            id
            ... on user {
              userName: name
            }
          }
        }
      `),
      schema,
    );

    expect(outputFields(result)).toEqual(['id', 'name']);
  });

  it('maps the nested selections onto nested object types', () => {
    const result = deriveAction(
      getOperation(`
        mutation insertUsers($objects: [user_insert_input!]!) {
          insert_user(objects: $objects) {
            count: affected_rows
            returning {
              id
              role
            }
          }
        }
      `),
      schema,
    );

    expect(result.action.output_type).toBe('InsertUsersOutput');
    const typesSdl = getTypesSdl(result.types);
    expect(typesSdl).toMatch(
      /type InsertUsersOutput\s*\{\s*affected_rows\s*:\s*Int!\s*returning\s*:\s*\[InsertUsersOutputReturning!\]!\s*\}/,
    );
    expect(typesSdl).toMatch(
      /type InsertUsersOutputReturning\s*\{\s*id\s*:\s*Int!\s*role\s*:\s*InsertUsersUserRole\s*\}/,
    );
    expect(typesSdl).toMatch(/enum InsertUsersUserRole\s*\{\s*admin\s*editor/);
  });

  it('rejects an operation without any field to map', () => {
    expect(() =>
      deriveAction(
        getOperation(`
          mutation insertUsers($objects: [user_insert_input!]!) {
            insert_user(objects: $objects) {
              __typename
            }
          }
        `),
        schema,
      ),
    ).toThrow();
  });

  it('rejects named fragments with a specific error', () => {
    expect(() =>
      deriveAction(
        getOperation(`
          query getUsers {
            user {
              ...userFields
            }
          }
        `),
        schema,
      ),
    ).toThrow('named fragments are not supported, use inline fragments');
  });

  it('rejects an inline fragment as the root of the operation', () => {
    expect(() =>
      deriveAction(
        getOperation(`
          query getUsers {
            ... on query_root {
              user {
                id
              }
            }
          }
        `),
        schema,
      ),
    ).toThrow('the root of the operation must be a field, not a fragment');
  });
});

describe('data connector scalars', () => {
  const mongoSchema = buildSchema(`
    scalar ObjectId
    scalar long
    scalar uuid

    type movies {
      _id: ObjectId!
      runtime: long
      external_id: uuid
      title: String
    }

    input movies_insert_input {
      _id: ObjectId
      title: String
    }

    type query_root {
      movies_by_pk(_id: ObjectId!): movies
    }

    type mutation_root {
      insert_movies_one(object: movies_insert_input!): movies
    }

    schema {
      query: query_root
      mutation: mutation_root
    }
  `);

  // shaped like the capabilities of the MongoDB data connector
  const scalarReplacements = getDataConnectorScalarReplacements([
    {
      scalar_types: {
        ObjectId: {},
        long: { graphql_type: 'String' },
        int: { graphql_type: 'Int' },
        // built-in names are never replaced
        String: { graphql_type: 'String' },
      },
    },
    undefined,
  ]);

  it('maps the scalars of data connectors to built-in scalars', () => {
    expect(scalarReplacements).toEqual({
      ObjectId: 'String',
      long: 'String',
      int: 'Int',
    });
  });

  it('replaces the data connector scalars of the output type and arguments', () => {
    const result = deriveAction(
      getOperation(`
        query getMovie($id: ObjectId!) {
          movies_by_pk(_id: $id) {
            _id
            runtime
            external_id
            title
          }
        }
      `),
      mongoSchema,
      null,
      scalarReplacements,
    );

    expect(result.action.arguments).toEqual([{ name: 'id', type: 'String!' }]);
    const typesSdl = getTypesSdl(result.types);
    expect(typesSdl).toMatch(/_id\s*:\s*String!/);
    expect(typesSdl).toMatch(/runtime\s*:\s*String/);
    // not declared by a data connector, e.g. a Postgres scalar
    expect(typesSdl).toMatch(/external_id\s*:\s*uuid/);
    expect(typesSdl).not.toContain('ObjectId');
  });

  it('replaces the data connector scalars of input object fields', () => {
    const result = deriveAction(
      getOperation(`
        mutation insertMovie($object: movies_insert_input!) {
          insert_movies_one(object: $object) {
            _id
          }
        }
      `),
      mongoSchema,
      null,
      scalarReplacements,
    );

    const typesSdl = getTypesSdl(result.types);
    expect(typesSdl).toContain('input InsertMovieMoviesInsertInput');
    expect(typesSdl).not.toContain('ObjectId');
  });
});
