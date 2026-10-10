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

// A valid TS identifier for the generated function name (handles anonymous ops).
const toFunctionSuffix = (name: string): string => {
  const cleaned = (name || '').replace(/[^A-Za-z0-9_$]/g, '');
  return /^[A-Za-z_$]/.test(cleaned) ? cleaned : 'Operation';
};

const indent = (value: string, spaces: number): string =>
  value.replace(/\n/g, `\n${' '.repeat(spaces)}`);

const typeScriptSnippet: Snippet = {
  name: `fetch`,
  language: `TypeScript`,
  codeMirrorMode: `jsx`,
  options: [],
  generate: ({ operationDataList, serverUrl, headers }) => {
    const queryDef = operationDataList[0];
    if (!queryDef) {
      return `// No operation selected — write a query/mutation to export a snippet.`;
    }

    // Everything user-controlled is JSON-serialised so the output is always
    // valid TypeScript regardless of quotes/backticks/${}/newlines in the
    // endpoint, query, operation name, or header values. Variables use the
    // operation's ACTUAL values (not undeclared identifiers as before).
    const allHeaders = { 'Content-Type': 'application/json', ...headers };
    const headersBlock = indent(JSON.stringify(allHeaders, null, 2), 4);
    const variablesBlock = indent(
      JSON.stringify(queryDef.variables ?? {}, null, 2),
      2,
    );
    const operationName = queryDef.name || '';
    const fnSuffix = toFunctionSuffix(operationName);
    // For an anonymous operation, send NO operationName: a GraphQL server (HGE
    // included) treats operationName `""` as "run the operation literally named
    // empty string" and errors, whereas an absent operationName runs the single
    // anonymous operation. `undefined` is dropped by JSON.stringify in the body.
    const operationNameArg = operationName
      ? JSON.stringify(operationName)
      : 'undefined';

    return `/*
This is an example snippet - you should consider tailoring it
to your service.

Note: we only handle the first operation here.
*/

async function fetchGraphQL(
  operationsDoc: string,
  operationName: string | undefined,
  variables: Record<string, unknown>,
) {
  const result = await fetch(${JSON.stringify(serverUrl)}, {
    method: 'POST',
    headers: ${headersBlock},
    body: JSON.stringify({
      query: operationsDoc,
      operationName,
      variables,
    }),
  });
  return result.json();
}

const operation = ${JSON.stringify(queryDef.query)};

function fetch${fnSuffix}() {
  return fetchGraphQL(operation, ${operationNameArg}, ${variablesBlock});
}

fetch${fnSuffix}()
  .then(({ data, errors }) => {
    if (errors) {
      console.error(errors);
    }
    console.log(data);
  })
  .catch((error) => {
    console.error(error);
  });
`;
  },
};

export default [...snippets, typeScriptSnippet];
