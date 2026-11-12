import { parse, parseType } from 'graphql';
import { getDerivedOutput } from './derive';
import { templater as nodejsExpress } from './nodejs-express';
import { templater as nodejsZeit } from './nodejs-zeit';
import { templater as nodejsAzureFunction } from './nodejs-azure-function';
import { templater as typescriptZeit } from './typescript-zeit';

// evaluates the generated expression against a Hasura response
const runDerivedOutput = (expression: string, data: unknown) =>
  new Function('data', `return ${expression};`)(data);

describe('getDerivedOutput', () => {
  const { definitions } = parse(`
    type GetUsersOutput {
      id: Int!
      email: String
    }

    type InsertUserOutput {
      id: Int!
      email: String
    }

    type InsertAlbumOutput {
      affected_rows: Int!
      returning: [InsertAlbumOutputReturning!]!
    }

    type InsertAlbumOutputReturning {
      ArtistId: Int!
      Title: String
      artist: InsertAlbumOutputReturningArtist
    }

    type InsertAlbumOutputReturningArtist {
      Name: String
    }
  `);

  it('reads the response under the alias of the root field', () => {
    const output = getDerivedOutput(
      'query getUsers { something: user { id email } }',
      parseType('[GetUsersOutput!]!'),
      definitions,
    );

    expect(output).toBe('data.something');
  });

  it('renames the aliased fields of a list back to the output type fields', () => {
    const output = getDerivedOutput(
      'query getUsers { something: user { alias1: id alias2: email } }',
      parseType('[GetUsersOutput!]!'),
      definitions,
    );

    expect(
      runDerivedOutput(output, {
        something: [
          { alias1: 1, alias2: 'a@hasura.io' },
          { alias1: 2, alias2: 'b@hasura.io' },
        ],
      }),
    ).toEqual([
      { id: 1, email: 'a@hasura.io' },
      { id: 2, email: 'b@hasura.io' },
    ]);
  });

  it('renames the aliased fields of an object back to the output type fields', () => {
    const output = getDerivedOutput(
      'mutation insertUser { insert_user_one(object: {}) { userId: id email } }',
      parseType('InsertUserOutput'),
      definitions,
    );

    expect(
      runDerivedOutput(output, {
        insert_user_one: { userId: 1, email: 'a@hasura.io' },
      }),
    ).toEqual({ id: 1, email: 'a@hasura.io' });
    expect(runDerivedOutput(output, { insert_user_one: null })).toBeNull();
  });

  it('renames the fields aliased inside inline fragments', () => {
    const output = getDerivedOutput(
      'query getUsers { user { id ... on user { userEmail: email } } }',
      parseType('[GetUsersOutput!]!'),
      definitions,
    );

    expect(
      runDerivedOutput(output, {
        user: [{ id: 1, userEmail: 'a@hasura.io' }],
      }),
    ).toEqual([{ id: 1, email: 'a@hasura.io' }]);
  });

  it('skips the output type fields that are not selected', () => {
    const output = getDerivedOutput(
      'query getUsers { user { userId: id } }',
      parseType('[GetUsersOutput!]!'),
      definitions,
    );

    expect(runDerivedOutput(output, { user: [{ userId: 1 }] })).toEqual([
      { id: 1 },
    ]);
  });

  it('reads nested objects as they are when nothing is aliased', () => {
    const output = getDerivedOutput(
      'mutation InsertAlbum { insert_albumMS(objects: []) { returning { ArtistId Title } } }',
      parseType('InsertAlbumOutput'),
      definitions,
    );

    expect(output).toBe('data.insert_albumMS');
  });

  it('renames the aliased fields of nested objects', () => {
    const output = getDerivedOutput(
      `mutation InsertAlbum {
        insert_albumMS(objects: []) {
          count: affected_rows
          returning {
            artistId: ArtistId
            Title
            artist { artistName: Name }
          }
        }
      }`,
      parseType('InsertAlbumOutput'),
      definitions,
    );

    expect(
      runDerivedOutput(output, {
        insert_albumMS: {
          count: 2,
          returning: [
            { artistId: 1, Title: 'A', artist: { artistName: 'X' } },
            { artistId: 2, Title: 'B', artist: null },
          ],
        },
      }),
    ).toEqual({
      affected_rows: 2,
      returning: [
        { ArtistId: 1, Title: 'A', artist: { Name: 'X' } },
        { ArtistId: 2, Title: 'B', artist: null },
      ],
    });
  });
});

const actionsSdl = `
type Query {
  getUsers: [GetUsersOutput!]!
}

type GetUsersOutput {
  id: Int!
  email: String
}
`;

describe.each([
  ['nodejs-express', nodejsExpress],
  ['nodejs-zeit', nodejsZeit],
  ['nodejs-azure-function', nodejsAzureFunction],
  ['typescript-zeit', typescriptZeit],
] as const)('%s codegen for a derived action', (_, templater) => {
  const generate = async (operation: string) =>
    (await templater('getUsers', actionsSdl, { operation, endpoint: '' }))[0]
      .content;

  it('returns the response under the alias of the root field', async () => {
    const content = await generate(
      'query getUsers { something: user { id email } }',
    );

    expect(content).toContain('data.something');
    expect(content).not.toContain('data.user');
    // a list response must not be spread into an object
    expect(content).not.toContain('...data.');
  });

  it('renames the aliased fields back to the output type fields', async () => {
    const content = await generate(
      'query getUsers { something: user { alias1: id alias2: email } }',
    );

    expect(content).toContain('data.something.map((row) => ({');
    expect(content).toContain('id: row.alias1');
    expect(content).toContain('email: row.alias2');
  });
});
