import { buildSchema, OperationDefinitionNode, parse } from 'graphql';
import deriveAction from './utils';

const schema = buildSchema(`
  type user {
    id: Int!
    name: String
    email: String
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
  }

  schema {
    query: query_root
    mutation: mutation_root
  }
`);

const getOperation = (query: string) =>
  parse(query).definitions[0] as OperationDefinitionNode;

const outputFields = (result: ReturnType<typeof deriveAction>) =>
  result.types
    .find((t: any) => t.name === result.action.output_type)
    .fields.map((f: { name: string }) => f.name);

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
    expect(outputFields(result)).toEqual(['id', 'name']);
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
              name
            }
          }
        }
      `),
      schema,
    );

    expect(outputFields(result)).toEqual(['id', 'name']);
  });
});
