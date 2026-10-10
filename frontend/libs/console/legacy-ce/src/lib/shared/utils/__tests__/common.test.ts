import { getActionDefinitionFromSdl } from '../sdlUtils';

describe('Get action definition by providing SDL', () => {
  it('Returns action definition for SDL input', () => {
    const mutationSDLInput = `type Mutation {
      actionName (arg1: SampleInput!): SampleOutput
    }`;

    const mutationSDLOutput = {
      name: 'actionName',
      arguments: [
        {
          name: 'arg1',
          type: 'SampleInput!',
          description: undefined,
        },
      ],
      outputType: 'SampleOutput',
      comment: undefined,
      error: null,
      type: 'mutation',
    };

    const emptyQuerySDLInput = `type Query`;
    const emptyQuerySDLOutput = {
      name: '',
      arguments: [],
      outputType: '',
      comment: '',
      error: null,
      type: 'query',
    };
    const querySDLInput = `type Query {
      queryName(username: String! password: String!): accessToken
    }`;
    const querySDLOutput = {
      name: 'queryName',
      arguments: [
        {
          name: 'username',
          type: 'String!',
          description: undefined,
        },
        {
          name: 'password',
          type: 'String!',
          description: undefined,
        },
      ],
      outputType: 'accessToken',
      comment: undefined,
      error: null,
      type: 'query',
    };

    expect(getActionDefinitionFromSdl(mutationSDLInput)).toStrictEqual(
      mutationSDLOutput,
    );
    expect(getActionDefinitionFromSdl(emptyQuerySDLInput)).toStrictEqual(
      emptyQuerySDLOutput,
    );
    expect(getActionDefinitionFromSdl(querySDLInput)).toStrictEqual(
      querySDLOutput,
    );
  });
});
