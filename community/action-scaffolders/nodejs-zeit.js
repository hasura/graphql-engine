const { buildClientSchema, printSchema, parse, isNonNullType, isListType, isWrappingType, isScalarType} = require('graphql');

const templater = (actionName, actionsSdl, derive) => {

  const ast = parse(`${actionsSdl}`);

  let mutationDef;
  const mutationAst = {
    ...ast,
    definitions: ast.definitions.filter(d => {
      if (d.name.value === 'Mutation') {
        if (mutationDef) return false
        mutationDef = d.fields.find(f => f.name.value === actionName);
        if (!mutationDef) {
          return false;
        } {
          return true;
        }
      }
      return false;
    })
  }
  const mutationName = mutationDef.name.value;
  const mutationArguments = mutationDef.arguments;

  let graphqlClientCode = '';
  let mutationCodegen = '';


  if (derive && derive.mutation && derive.mutation.name) {
    const getSampleValue = (typename) => {
      switch(typename) {
        case 'String':
          return 'sample value';
        case 'Int':
          return ''
        case 'uuid':
          return '66e7a19a-6d5b-4851-b6e0-ea14a6f32cff';
        case 'date':
          return '2019-12-11';
        case 'timestamptz':
          return '2019-12-11T13:55:45.070803+00:00'
        default:
          return 'sample value'
      }
    };
    const refMutationName = derive.mutation.name;
    const hasuraSchema = buildClientSchema(derive.introspection_schema);
    const mutationType = hasuraSchema._mutationType;
    const allMutations = mutationType._fields;
    const refMutation = allMutations[refMutationName];
    const nonNullableArguments = {};
    refMutation.args.forEach(a => {
      if (!isNonNullType(a.type)) return;
      let isList = false;
      const argFields = {};
      let _type = Object.assign(Object.create(a.type), a.type);
      while (isWrappingType(_type)) {
        if (isListType(_type)) {
          isList = true;
        }
        _type = _type.ofType;
      }
      if (_type._fields) {
        Object.values(_type._fields).forEach(f => {
          if (isScalarType(f.type)) {
            argFields[f.name] = getSampleValue(f.type.name)
          }
        })
      }
      nonNullableArguments[a.name] = {
        name: a.name,
        type: _type.name,
        isList,
        argFields
      }
    })

    const varDefCodegen = Object.values(nonNullableArguments).map(a => {
      const argType = a.isList ? `[${a.type}!]!` : `a.type`
      return `    $_${a.name}: ${argType}`
    }).join('\n');

    const mutationArgCodegen = Object.values(nonNullableArguments).map(a => {
      const argType = a.isList ? `[${a.type}!]!` : `a.type`
      return `      ${a.name}: $_${a.name}`
    }).join('\n');

    mutationCodegen = `
const HASURA_MUTATION = \`
  mutation (
${varDefCodegen}
  ) {
    ${refMutationName} (
${mutationArgCodegen}
    ) {
      affected_rows
    }
  }
\`;
`;

  const varCodegen = Object.values(nonNullableArguments).map(a => {
    const varValue = a.isList ? `[${JSON.stringify(a.argFields)}]` : JSON.stringify(a.argFields);
    return `          "_${a.name}": ${varValue}`
  }).join(',\n');

  graphqlClientCode = `
  let response = await fetch(
    'http://localhost:8080/v1/graphql',
    {
      method: 'POST',
      body: JSON.stringify({
        query: HASURA_MUTATION,
        variables: {
${varCodegen}
        }
      })
    }
  )

  let responseBody = await response.json();
`

  }

  const handlerContent = `
${derive ? 'const fetch = require("node-fetch")' : ''}
${derive ? mutationCodegen : ''}
const handler = async (req, res) => {
${derive ? graphqlClientCode : ''}  

  /*

  In case of errors:
  
  return res.status(400).json({
    errors: {
      code: '<error code>',
      message: "error happened"
    }
  })

  */

  return res.json({ data: {} })
}

module.exports = handler;
`;

  const handlerFile = {
    name: `${mutationName}.js`,
    content: handlerContent
  }

  return [handlerFile];

}

module.exports = templater;