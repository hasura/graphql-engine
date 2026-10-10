import { parse } from 'graphql';
import { CodegenDerive, CodegenFile } from './types';

// Generate the basic handler
// For azure
const generateHandler = (
  extractArgsFromBody: string,
  outputTypeSpread: string,
) => {
  const handlerBeginningCode = `
module.exports = async function (context, req) {

  ${extractArgsFromBody}

  // Write business logic that deals with inputs here...

`;

  const errorSuccessHandlerResponseCode = `
  // If error:
  // context.res = {
  //   status: 400,
  //   body: { code: "internal-error", message: "Internal error." }
  // };
  // return;

  context.res = {
    headers: { 'Content-Type': 'application/json' },
    body: ${outputTypeSpread}
  };
};`;
  return { handlerBeginningCode, errorSuccessHandlerResponseCode };
};

// Template code for generating a fetch API call
// Only used for derive
const generateFetch = (
  actionName: string,
  rawQuery: string,
  variables: string,
  queryRootFieldName: string,
  derive: { operation: string; endpoint?: string },
) => {
  const queryName = 'HASURA_' + actionName.toUpperCase();
  const actionNameUpper = actionName[0].toUpperCase() + actionName.slice(1);

  const fetchExecuteCode = `
const fetch = require ('node-fetch');

const ${queryName} = \`
${rawQuery}
\`;

const execute${actionNameUpper} = async (variables) => {
  const result = await fetch ("${derive.endpoint || 'http://localhost:8080/v1/graphql'}", {
    method: 'POST',
    body: JSON.stringify({
      query: ${queryName},
      variables
    })
  });

  const data = await result.json();
  console.log('DEBUG: ', data);
  return data;
};
`;

  const runExecuteInHandlerCode = `
  // Execute the Hasura query
  const {data, errors} = await execute${actionNameUpper}(${variables}, headers);

  // If there's an error in the running the Hasura query
  if (errors) {
    context.res = {
      headers: { 'Content-Type': 'application/json' },
      status: 400,
      body: errors[0]
    };
    return;
  }

  // If success
  context.res = {
    headers: { 'Content-Type': 'application/json' },
    body: {
      ...data.${queryRootFieldName}
    }
  };
};`;

  return { fetchExecuteCode, runExecuteInHandlerCode };
};

// actionName: Name of the action
// actionsSdl: GraphQL SDL string that has the action and dependent types
// derive: Whether this action was asked to be derived from a Hasura operation
//         derive.operation contains the operation string
export const templater = (
  actionName: string,
  actionsSdl: string,
  derive: CodegenDerive,
): CodegenFile[] => {
  // Parse the actions SDL into an AST
  const ast: any = parse(actionsSdl);

  // Find the type for this action
  let actionDef: any;
  for (let i = ast.definitions.length - 1; i >= 0; i--) {
    const typeDef = ast.definitions[i];
    if (typeDef.name.value === 'Mutation' || typeDef.name.value === 'Query') {
      actionDef = typeDef.fields.find(
        (def: any) => def.name.value === actionName,
      );
      if (actionDef) {
        break;
      }
    }
  }

  // If the input arguments are {name, age, email}
  // then we want to generate: const {name, age, email} = req.body
  const inputArgumentsNames = actionDef.arguments.map((i: any) => i.name.value);

  const extractArgsFromBody = `const {${inputArgumentsNames.join(', ')}} = req.body.input;`;

  // If the output type is type ActionResult {field1: <>, field2: <>}
  // we want to template the response of the handler to be:
  // {
  //    field1: "",
  //    field2: ""
  // }
  const actionOutputType = ast.definitions.find(
    (def: any) => def.name.value === actionDef.type.name.value,
  );
  const outputTypeFieldNames = actionOutputType.fields.map(
    (f: any) => f.name.value,
  );

  let outputTypeSpread = '{\n      ';
  outputTypeFieldNames.forEach((n: string, i: number) => {
    outputTypeSpread += n + ': ""';
    if (i === outputTypeFieldNames.length - 1) {
      outputTypeSpread += '\n    }';
    } else {
      outputTypeSpread += ',\n      ';
    }
  });

  const basicHandlerCode = generateHandler(
    extractArgsFromBody,
    outputTypeSpread,
  );

  // If this action is being derived for an existing operation
  // then we'll add a fetch API call
  let deriveCode: ReturnType<typeof generateFetch> | undefined;
  const isDerivation = !!(derive && derive.operation);
  if (isDerivation && derive) {
    const operationAST: any = parse(derive.operation);
    const queryRootField =
      operationAST.definitions[0].selectionSet.selections.find(
        (f: any) => !f.name.value.startsWith('__'),
      );
    const queryRootFieldName = queryRootField.alias
      ? queryRootField.alias.value
      : queryRootField.name.value;
    const variableNames = operationAST.definitions[0].variableDefinitions.map(
      (vdef: any) => vdef.variable.name.value,
    );

    deriveCode = generateFetch(
      actionName,
      derive.operation,
      `{ ${variableNames.join(', ')} }`,
      queryRootFieldName,
      derive,
    );
  }

  // Render the handler!
  let finalHandlerCode = '';
  if (!isDerivation) {
    finalHandlerCode +=
      basicHandlerCode.handlerBeginningCode +
      basicHandlerCode.errorSuccessHandlerResponseCode;
  } else if (deriveCode) {
    finalHandlerCode +=
      deriveCode.fetchExecuteCode +
      basicHandlerCode.handlerBeginningCode +
      deriveCode.runExecuteInHandlerCode;
  }

  return [
    {
      name: actionName + '.js',
      content: finalHandlerCode,
    },
  ];
};
