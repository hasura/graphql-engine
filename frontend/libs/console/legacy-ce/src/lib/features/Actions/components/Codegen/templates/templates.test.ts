import { templater as nodejsExpress } from './nodejs-express';
import { templater as nodejsZeit } from './nodejs-zeit';
import { templater as typescriptZeit } from './typescript-zeit';
import { templater as nodejsAzureFunction } from './nodejs-azure-function';
import { CodegenFile } from './types';

const scalarOutputSdl = `
type Query {
  action_name(
    actionInput: [String!]
  ): String!
}
`;

const objectOutputSdl = `
type Mutation {
  insertUser(name: String!): InsertUserOutput!
}

type InsertUserOutput {
  id: Int!
  name: String!
}
`;

const templaters: [
  string,
  (
    actionName: string,
    sdl: string,
    derive: any,
  ) => CodegenFile[] | Promise<CodegenFile[]>,
][] = [
  ['nodejs-express', nodejsExpress],
  ['nodejs-zeit', nodejsZeit],
  ['typescript-zeit', typescriptZeit],
  ['nodejs-azure-function', nodejsAzureFunction],
];

describe.each(templaters)('%s codegen', (_, templater) => {
  it('generates a handler for an action with a scalar output', async () => {
    const files = await templater('action_name', scalarOutputSdl, null);
    const handler = files[0].content;

    expect(handler).toContain('actionInput');
    expect(handler).toMatch(/(res\.json\("<value>"\)|body: "")/);
  });

  it('generates field placeholders for an action with an object output', async () => {
    const files = await templater('insertUser', objectOutputSdl, null);
    const handler = files[0].content;

    expect(handler).toMatch(/id: ""|id: "<value>"/);
    expect(handler).toMatch(/name: ""|name: "<value>"/);
  });
});
