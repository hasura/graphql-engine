import { parse } from 'graphql';
import { CodegenDerive, CodegenFile } from './types';

export const templater = (
  actionName: string,
  actionsSdl: string,
  derive: CodegenDerive,
): CodegenFile[] => {
  const ast: any = parse(`${actionsSdl}`);

  let actionDef: any;

  ast.definitions.filter((d: any) => {
    if (
      (d.name.value === 'Mutation' || d.name.value === 'Query') &&
      (d.kind === 'ObjectTypeDefinition' || d.kind === 'ObjectTypeExtension')
    ) {
      if (actionDef) return false;
      actionDef = d.fields.find((f: any) => f.name.value === actionName);
      if (!actionDef) {
        return false;
      } else {
        return true;
      }
    }
    return false;
  });

  let actionOutputType = actionDef.type;

  while (actionOutputType.kind !== 'NamedType') {
    actionOutputType = actionOutputType.type;
  }
  const outputType = ast.definitions.find((d: any) => {
    return (
      d.kind === 'ObjectTypeDefinition' &&
      d.name.value === actionOutputType.name.value
    );
  });

  const outputTypeFields = outputType.fields.map((f: any) => f.name.value);

  let graphqlClientCode = '';
  let operationCodegen = '';
  let errorSnippet = '';
  let successSnippet = '';
  let executeFunction = '';

  const requestInputDestructured = `{ ${actionDef.arguments
    .map((a: any) => a.name.value)
    .join(', ')} }`;

  const shouldDerive = !!(derive && derive.operation);
  const hasuraEndpoint =
    derive && derive.endpoint
      ? derive.endpoint
      : 'http://localhost:8080/v1/graphql';
  if (shouldDerive && derive) {
    const operationDoc: any = parse(derive.operation);
    const operationName =
      operationDoc.definitions[0].selectionSet.selections.filter(
        (s: any) => s.name.value.indexOf('__') !== 0,
      )[0].name.value;

    operationCodegen = `
const HASURA_OPERATION = \`
${derive.operation}
\`;`;

    executeFunction = `
// execute the parent operation in Hasura
const execute = async (variables) => {
  const fetchResponse = await fetch(
    "${hasuraEndpoint}",
    {
      method: 'POST',
      body: JSON.stringify({
        query: HASURA_OPERATION,
        variables
      })
    }
  );
  const data = await fetchResponse.json();
  console.log('DEBUG: ', data);
  return data;
};
  `;

    graphqlClientCode = `
  // execute the Hasura operation
  const { data, errors } = await execute(${requestInputDestructured});`;

    errorSnippet = `  // if Hasura operation errors, then throw error
  if (errors) {
    return res.status(400).json(errors[0])
  }`;

    successSnippet = `  // success
  return res.json({
    ...data.${operationName}
  })`;
  }

  if (!errorSnippet) {
    errorSnippet = `  /*
  // In case of errors:
  return res.status(400).json({
    message: "error happened"
  })
  */`;
  }

  if (!successSnippet) {
    successSnippet = `  // success
  return res.json({
${outputTypeFields.map((f: string) => `    ${f}: "<value>"`).join(',\n')}
  })`;
  }

  const handlerContent = `
${shouldDerive ? 'const fetch = require("node-fetch")\n' : ''}${
    shouldDerive ? `${operationCodegen}\n` : ''
  }${shouldDerive ? `${executeFunction}\n` : ''}
// Request Handler
const handler = async (req, res) => {

  // get request input
  const ${requestInputDestructured} = req.body.input;

  // run some business logic
${shouldDerive ? graphqlClientCode : ''}

${errorSnippet}

${successSnippet}

};

module.exports = handler;
`;

  const handlerFile: CodegenFile = {
    name: `${actionName}.js`,
    content: handlerContent,
  };

  return [handlerFile];
};
