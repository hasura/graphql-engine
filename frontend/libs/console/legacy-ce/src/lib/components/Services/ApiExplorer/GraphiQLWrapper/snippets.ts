import snippets from 'graphiql-code-exporter/lib/snippets';
import { OperationTypeNode, OperationDefinitionNode } from 'graphql';

export type Options = Array<{ id: string; label: string; initial: boolean }>;

export type OptionValues = { [id: string]: boolean };

export type OperationData = {
  query: string;
  name: string;
  displayName: string;
  type: OperationTypeNode;
  variableName: string;
  variables: Record<string, unknown>;
  operationDefinition: OperationDefinitionNode;
};

export type GenerateOptions = {
  serverUrl: string;
  headers: { [name: string]: string };
  context: Record<string, unknown>;
  operationDataList: Array<OperationData>;
  options: OptionValues;
};

export type CodesandboxFile = {
  content: string;
};

export type CodesandboxFiles = {
  [filename: string]: CodesandboxFile;
};

export type Snippet = {
  options: Options;
  language: string;
  codeMirrorMode: string;
  name: string;
  generate: (options: GenerateOptions) => string;
  generateCodesandboxFiles?: (options: GenerateOptions) => CodesandboxFiles;
};

const getQuery = (query: string) => {
  return ` `.repeat(2) + query.replace(/\n/g, `\n${` `.repeat(2)}`);
};

const getVariables = (operationData: OperationData): string => {
  const params = (
    operationData?.operationDefinition?.variableDefinitions || []
  ).map(def => def?.variable?.name?.value);
  const variablesBody = params.map(param => `"${param}": ${param}`).join(', ');
  const variables = `{${variablesBody}}`;
  return variables;
};

const typeScriptSnippet: Snippet = {
  name: `fetch`,
  language: `TypeScript`,
  codeMirrorMode: `jsx`,
  options: [],
  generate: ({ operationDataList }) => {
    const queryDef = operationDataList[0];

    return `
/*
This is an example snippet - you should consider tailoring it
to your service.

Note: we only handle the first operation here
*/

function fetchGraphQL(
  operationsDoc: string,
  operationName: string,
  variables: Record<string, any>
) {
  return fetch('undefined', {
    method: 'POST',
    body: JSON.stringify({
      query: operationsDoc,
      variables,
      operationName,
    }),
  }).then(result => result.json());
}

const operation = \`
${getQuery(queryDef.query)}
\`;

function fetch${queryDef.name}() {
  return fetchGraphQL(operations, ${queryDef.name}, ${getVariables(queryDef)})
}

fetch${queryDef.name}()
  .then(({ data, errors }) => {
    if (errors) {
      console.error(errors);
    }
    console.log(data);
  })
  .catch(error => {
    console.error(error);
  });
`;
  },
};

export default [...snippets, typeScriptSnippet];
