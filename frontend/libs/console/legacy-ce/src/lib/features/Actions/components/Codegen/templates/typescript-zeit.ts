import { parse } from 'graphql';
import { camelize } from 'inflection';
import { CodegenDerive, CodegenFile } from './types';
import { generateHasuraTypes } from './generateHasuraTypes';

export const templater = async (
  actionName: string,
  actionsSdl: string,
  derive: CodegenDerive,
): Promise<CodegenFile[]> => {
  const ast: any = parse(`${actionsSdl}`);

  const allMutationActionDefs = ast.definitions.filter(
    (d: any) => d.name.value === 'Mutation',
  );
  const allQueryActionDefs = ast.definitions.filter(
    (d: any) => d.name.value === 'Query',
  );
  let allMutationActionFields: any[] = [];
  allMutationActionDefs.forEach((md: any) => {
    allMutationActionFields = [...allMutationActionFields, ...md.fields];
  });
  let allQueryActionFields: any[] = [];
  allQueryActionDefs.forEach((qd: any) => {
    allQueryActionFields = [...allQueryActionFields, ...qd.fields];
  });

  const typesCodegen = generateHasuraTypes(ast, {
    Mutation: allMutationActionFields,
    Query: allQueryActionFields,
  });
  const typesFileMetadata: CodegenFile = {
    content: typesCodegen,
    name: `hasuraCustomTypes.ts`,
  };

  let actionDef: any;
  let actionType = '';
  ast.definitions.filter((d: any) => {
    if (d.name.value === 'Mutation' || d.name.value === 'Query') {
      if (actionDef) return false;
      actionDef = d.fields.find((f: any) => f.name.value === actionName);
      actionType = d.name.value;
      if (!actionDef) {
        return false;
      } else {
        return true;
      }
    }
    return false;
  });

  const actionArgType = `${actionType}${camelize(actionName)}Args`;

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
const HASURA_OPERATION = \`${derive.operation}\`;`;

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

  const handlerContent = `import { NowRequest, NowResponse } from '@now/node';
import { ${actionArgType} } from './hasuraCustomTypes';
${derive ? 'import fetch from "node-fetch"\n' : ''}${derive ? `${operationCodegen}\n` : ''}${derive ? `${executeFunction}\n` : ''}
// Request Handler
const handler = async (req: NowRequest, res: NowResponse) => {

  // get request input
  const ${requestInputDestructured}: ${actionArgType} = req.body.input;

  // run some business logic
${derive ? graphqlClientCode : ''}

${errorSnippet}

${successSnippet}

};

export default handler;
`;

  const handlerFileMetadata: CodegenFile = {
    name: `${actionName}.ts`,
    content: handlerContent,
  };

  return [handlerFileMetadata, typesFileMetadata];
};
