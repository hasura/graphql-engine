import { templater as nodejsExpress } from './nodejs-express';
import { templater as nodejsZeit } from './nodejs-zeit';
import { templater as typescriptZeit } from './typescript-zeit';

const actionsSdl = `
type Query {
  getUsers: GetUsersOutput
}

type GetUsersOutput {
  id: Int!
}
`;

describe.each([
  ['nodejs-express', nodejsExpress],
  ['nodejs-zeit', nodejsZeit],
  ['typescript-zeit', typescriptZeit],
] as const)('%s codegen for a derived action', (_, templater) => {
  it('reads the response data under the alias of the root field', async () => {
    const files = await templater('getUsers', actionsSdl, {
      operation: 'query getUsers { something: user { id } }',
      endpoint: '',
    });

    expect(files[0].content).toContain('...data.something');
    expect(files[0].content).not.toContain('...data.user');
  });
});
