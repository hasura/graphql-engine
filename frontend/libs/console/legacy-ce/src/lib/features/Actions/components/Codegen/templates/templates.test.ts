import { templater as nodejsExpress } from './nodejs-express';
import { templater as nodejsZeit } from './nodejs-zeit';
import { templater as typescriptZeit } from './typescript-zeit';
import { templater as nodejsAzureFunction } from './nodejs-azure-function';
import { templater as typescriptExpress } from './typescript-express';
import { templater as javascriptJSDocExpress } from './javascript-js-doc-express';
import { templater as goServeMux } from './go-serve-mux';
import { CodegenFile } from './types';

// the console passes the action definitions as `extend type Query/Mutation`
const scalarOutputSdl = `
extend type Query {
  action_name(
    actionInput: [String!]
  ): String!
}
`;

const customScalarOutputSdl = `
extend type Query {
  action_name(actionInput: [String!]): uuid
}

scalar uuid
`;

const enumOutputSdl = `
extend type Query {
  action_name(actionInput: [String!]): Kind!
}

enum Kind {
  ADMIN
  USER
}
`;

const objectOutputSdl = `
extend type Mutation {
  insertUser(name: String!): InsertUserOutput!
}

type InsertUserOutput {
  id: Int!
  name: String!
}
`;

const listOutputSdl = `
extend type Query {
  listUsers: [InsertUserOutput!]!
}

type InsertUserOutput {
  id: Int!
  name: String!
}
`;

const derive = {
  operation:
    'mutation insertUser($name: String!) { insert_user_one(object: {name: $name}) { id name } }',
  endpoint: 'https://my-hasura.app/v1/graphql',
};

type Templater = (
  actionName: string,
  sdl: string,
  derive: any,
) => CodegenFile[] | Promise<CodegenFile[]>;

const templaters: [
  string,
  Templater,
  {
    scalarOutput: string;
    enumOutput: string;
    objectOutputFields: string[];
    listOutput: string;
  },
][] = [
  [
    'nodejs-express',
    nodejsExpress,
    {
      scalarOutput: 'res.json("<value>")',
      enumOutput: 'res.json("<value>")',
      objectOutputFields: ['id: "<value>"', 'name: "<value>"'],
      listOutput: 'return res.json([\n    {\n      id: "<value>",',
    },
  ],
  [
    'nodejs-zeit',
    nodejsZeit,
    {
      scalarOutput: 'res.json("<value>")',
      enumOutput: 'res.json("<value>")',
      objectOutputFields: ['id: "<value>"', 'name: "<value>"'],
      listOutput: 'return res.json([\n    {\n      id: "<value>",',
    },
  ],
  [
    'typescript-zeit',
    typescriptZeit,
    {
      scalarOutput: 'res.json("<value>")',
      enumOutput: 'res.json("<value>")',
      objectOutputFields: ['id: "<value>"', 'name: "<value>"'],
      listOutput: 'return res.json([\n    {\n      id: "<value>",',
    },
  ],
  [
    'nodejs-azure-function',
    nodejsAzureFunction,
    {
      scalarOutput: 'body: ""',
      enumOutput: 'body: ""',
      objectOutputFields: ['id: ""', 'name: ""'],
      listOutput: 'body: [\n      {\n        id: "",',
    },
  ],
  [
    'typescript-express',
    typescriptExpress,
    {
      scalarOutput: 'return "<sample value>"',
      enumOutput: 'return "<sample value>"',
      objectOutputFields: ['id: 1111', 'name: "<sample value>"'],
      listOutput: 'return [\n    {\n      id: 1111,',
    },
  ],
  [
    'javascript-js-doc-express',
    javascriptJSDocExpress,
    {
      scalarOutput: 'return "<sample value>"',
      enumOutput: 'return "<sample value>"',
      objectOutputFields: ['id: 1111', 'name: "<sample value>"'],
      listOutput: 'return [\n    {\n      id: 1111,',
    },
  ],
  [
    'go-serve-mux',
    goServeMux,
    {
      scalarOutput: 'response = "<sample value>"',
      enumOutput: 'response = KindADMIN',
      objectOutputFields: ['Id: 1111', 'Name: "<sample value>"'],
      listOutput:
        'response = []InsertUserOutput{\n    InsertUserOutput{\n      Id: 1111,',
    },
  ],
];

describe.each(templaters)('%s codegen', (_, templater, expected) => {
  it.each([
    ['a scalar', scalarOutputSdl],
    ['a custom scalar', customScalarOutputSdl],
  ])('generates a handler for an action with %s output', async (__, sdl) => {
    const files = await templater('action_name', sdl, null);
    const handler = files[0].content;

    expect(handler).toContain(expected.scalarOutput);
  });

  it('generates a handler for an action with an enum output', async () => {
    const files = await templater('action_name', enumOutputSdl, null);

    expect(files[0].content).toContain(expected.enumOutput);
  });

  it('generates a list of placeholders for an action with a list output', async () => {
    const files = await templater('listUsers', listOutputSdl, null);

    expect(files[0].content).toContain(expected.listOutput);
  });

  it('generates field placeholders for an action with an object output', async () => {
    const files = await templater('insertUser', objectOutputSdl, null);
    const handler = files[0].content;

    expected.objectOutputFields.forEach((field) => {
      expect(handler).toContain(field);
    });
  });
});

describe.each([
  ['typescript-express', typescriptExpress],
  ['javascript-js-doc-express', javascriptJSDocExpress],
  ['go-serve-mux', goServeMux],
] as const)('%s derived codegen', (_, templater) => {
  it('requests the configured Hasura endpoint', () => {
    const [handler] = templater('insertUser', objectOutputSdl, derive);

    expect(handler.content).toContain('https://my-hasura.app/v1/graphql');
    expect(handler.content).not.toContain('localhost');
  });
});

describe('typescript-express codegen', () => {
  it('types the handler with the TypeScript type of a scalar output', () => {
    const [handler] = typescriptExpress('action_name', scalarOutputSdl, null);

    expect(handler.content).toContain(
      'function action_nameHandler(args: action_nameArgs): string {',
    );
  });

  it('generates the types of the actions SDL', () => {
    const [, types] = typescriptExpress('insertUser', objectOutputSdl, null);

    expect(types.content).toContain(
      'type InsertUserOutput = {\n  id: number\n  name: string\n}',
    );
    expect(types.content).toContain(
      'type insertUserArgs = {\n  name: string\n}',
    );
  });

  it('keeps the nullability of each level of a list type', () => {
    const [, types] = typescriptExpress(
      'action_name',
      `extend type Query { action_name(a: [String!], b: [[Int]!]!): String }`,
      null,
    );

    expect(types.content).toContain(
      'type action_nameArgs = {\n  a?: Maybe<Array<string>>\n  b: Array<Array<Maybe<number>>>\n}',
    );
  });
});

describe('go-serve-mux codegen', () => {
  it('returns the declared Go type of a scalar output', () => {
    const [scalarHandler] = goServeMux('action_name', scalarOutputSdl, null);
    const [customScalarHandler] = goServeMux(
      'action_name',
      customScalarOutputSdl,
      null,
    );

    expect(scalarHandler.content).toContain(
      'func action_name(args action_nameArgs) (response string, err error) {',
    );
    expect(customScalarHandler.content).toContain(
      'func action_name(args action_nameArgs) (response Uuid, err error) {',
    );
  });

  it('imports only the packages that the handler uses', () => {
    const [handler] = goServeMux('insertUser', objectOutputSdl, null);
    const [derivedHandler] = goServeMux('insertUser', objectOutputSdl, derive);

    expect(handler.content).toContain(
      'import (\n  "encoding/json"\n  "io"\n  "log"\n  "net/http"\n)',
    );
    expect(derivedHandler.content).toContain(
      'import (\n  "bytes"\n  "encoding/json"\n  "errors"\n  "io"\n  "log"\n  "net/http"\n)',
    );
  });

  it('generates the types in package main with JSON field names', () => {
    const [, types] = goServeMux('action_name', enumOutputSdl, null);
    const [, objectTypes] = goServeMux('insertUser', objectOutputSdl, null);

    expect(types.content.startsWith('package main\n')).toBe(true);
    expect(types.content).toContain(
      'const (\n    KindADMIN Kind = "ADMIN"\n    KindUSER Kind = "USER"\n)',
    );
    expect(objectTypes.content).toContain(
      'type InsertUserOutput struct {\n    Id int `json:"id"`\n    Name string `json:"name"`\n}',
    );
  });

  it('uses the derived operation root field', () => {
    const [handler] = goServeMux('insertUser', objectOutputSdl, derive);

    expect(handler.content).toContain(
      'Insert_user_one InsertUserOutput `json:"insert_user_one"`',
    );
    expect(handler.content).toContain(
      'response = hasuraResponse.Data.Insert_user_one',
    );
  });
});
