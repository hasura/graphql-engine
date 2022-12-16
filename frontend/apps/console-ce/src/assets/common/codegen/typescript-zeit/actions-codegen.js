/*Utils*/

const getWrappingTypeMetadata = (_type) => {
  let type = JSON.parse(JSON.stringify(_type));
  const wrapperStack = [];
  while (type.kind !== 'NamedType') {
    if (type.kind === 'ListType') {
      wrapperStack.push('l');
    }
    if(type.kind === 'NonNullType') {
      wrapperStack.push('n');
    }
    type = type.type;
  }
  const typename = type.name.value;
  return {
    typename,
    stack: wrapperStack.reverse()
  }
}

const getTypescriptTypename = (_typename, wrapperStack) => {
  let typename = _typename;
  if (!wrapperStack.length || wrapperStack[0] === 'l') {
    typename = `Maybe<${typename}>`
  }
  wrapperStack.forEach((w, i) => {
    if (w === 'l') {
      if (wrapperStack[i+1] === 'n') {
        typename = `Array <${typename}>`
      } else {
        typename = `Maybe <Array<${typename}>>`
      }
    }
  });
  return typename;
}

const getHasuraScalars = (ast) => {
  let hasuraScalars = {};
  ast.definitions.forEach(d => {
    if (d.fields) {
      d.fields.forEach(f => {
        const fieldTypeMetadata = getWrappingTypeMetadata(f.type);
        if (!ast.definitions.some(dd => dd.name.value === fieldTypeMetadata.typename)) {
          hasuraScalars[fieldTypeMetadata.typename] = true;
        }
        if (f.arguments) {
          f.arguments.forEach(a => {
            const argTypeMetadata = getWrappingTypeMetadata(a.type);
            if (!ast.definitions.some(dd => dd.name.value === argTypeMetadata.typename)) {
              hasuraScalars[argTypeMetadata.typename] = true;
            }
          })
        }
      })
    }
  });
  return Object.keys(hasuraScalars)
};

const templater = async (actionName, actionsSdl, derive) => {

  const ast = parse(`${actionsSdl}`);

  const typesAst = {
    ...ast,
    definitions: ast.definitions.filter(d => (d.name.value !== 'Mutation' && d.name.value !== 'Query'))
  };

  const allMutationActionDefs = ast.definitions.filter(d => (d.name.value === 'Mutation'));
  const allQueryActionDefs = ast.definitions.filter(d => (d.name.value === 'Query'));
  let allMutationActionFields = [];
  allMutationActionDefs.forEach(md => {
    allMutationActionFields = [...allMutationActionFields, ...md.fields]
  });
  let allQueryActionFields = [];
  allQueryActionDefs.forEach(qd => {
    allQueryActionFields = [...allQueryActionFields, ...qd.fields]
  });

  // TODO (this is a temporary hack, must be fixed by accepting hasura scalars as a parameter)
  const hasuraScalars = getHasuraScalars(ast);
  const scalarsSdl = hasuraScalars.map(s => {
    return `scalar ${s}`;
  }).join('\n');
  const scalarsAst = parse(scalarsSdl);
  if (hasuraScalars.length) {
    typesAst.definitions.push(...scalarsAst.definitions);
  }

  const mutationRootDef = ast.definitions.find(d => d.name.value === 'Mutation');
  const queryRootDef = ast.definitions.find(d => d.name.value === 'Query');

  if (mutationRootDef) {
    mutationRootDef.kind = 'ObjectTypeDefinition';
    mutationRootDef.fields = allMutationActionFields;
    typesAst.definitions.push(mutationRootDef);
  }

  if (queryRootDef) {
    queryRootDef.kind = 'ObjectTypeDefinition';
    queryRootDef.fields = allQueryActionFields;
    typesAst.definitions.push(queryRootDef);
  }

  const codegenConfig = {
    schema: typesAst,
    plugins: [
      {
        typescript: {},
      },
    ],
    pluginMap: {
      typescript: typescriptPlugin
    }
  }
  const typesCodegen = await codegen(codegenConfig);
  const typesFileMetadata = {
    content: typesCodegen,
    name: `hasuraCustomTypes.ts`
  }

  let actionDef;
  let actionType = '';
  const actionAst = {
    ...ast,
    definitions: ast.definitions.filter(d => {
      if (d.name.value === 'Mutation' || d.name.value === 'Query') {
        if (actionDef) return false
        actionDef = d.fields.find(f => f.name.value === actionName);
        actionType = d.name.value;
        if (!actionDef) {
          return false;
        } else {
          return true;
        }
      }
      return false;
    })
  }

  const actionArgType = (`${actionType}${camelize(actionName)}Args`)

  const actionArguments = actionDef.arguments;
  let actionOutputType = actionDef.type;

  while (actionOutputType.kind !== 'NamedType') {
    actionOutputType = actionOutputType.type;
  }
  const outputType = ast.definitions.find(d => {
    return (d.kind === 'ObjectTypeDefinition' && d.name.value === actionOutputType.name.value)
  });

  const outputTypeFields = outputType.fields.map(f => f.name.value);

  let graphqlClientCode = '';
  let operationCodegen = '';
  let validateFunction = '';
  let errorSnippet = '';
  let successSnippet = '';
  let executeFunction = '';

  const requestInputDestructured = `{ ${actionDef.arguments.map(a => a.name.value).join(', ')} }`;

  const shouldDerive = !!(derive && derive.operation)
  const hasuraEndpoint = derive && derive.endpoint ? derive.endpoint : 'http://localhost:8080/v1/graphql';
  if (shouldDerive) {

    const operationDoc = parse(derive.operation);
    const operationName = operationDoc.definitions[0].selectionSet.selections.filter(s => s.name.value.indexOf('__') !== 0)[0].name.value;

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
  `

    graphqlClientCode = `
  // execute the Hasura operation
  const { data, errors } = await execute(${requestInputDestructured});`

    errorSnippet = `  // if Hasura operation errors, then throw error
  if (errors) {
    return res.status(400).json(errors[0])
  }`;

    successSnippet = `  // success
  return res.json({
    ...data.${operationName}
  })`

  }

  if (!errorSnippet) {
    errorSnippet = `  /*
  // In case of errors:
  return res.status(400).json({
    message: "error happened"
  })
  */`
  }

  if (!successSnippet) {
    successSnippet = `  // success
  return res.json({
${outputTypeFields.map(f => `    ${f}: "<value>"`).join(',\n')}
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

  const handlerFileMetadata = {
    name: `${actionName}.ts`,
    content: handlerContent
  }

  return [handlerFileMetadata, typesFileMetadata];
}
