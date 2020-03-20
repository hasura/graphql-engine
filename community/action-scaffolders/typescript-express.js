const { buildSchema, printSchema, parse } = require('graphql');
const { codegen } = require('@graphql-codegen/core');
const typescriptPlugin = require('@graphql-codegen/typescript');
const { camelize } = require('inflection');

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

/*Templater*/

const templater = async (
  actionName,
  actionsSdl,
  derive
) => {
  const ast = parse(`${actionsSdl}`);
  const typesAst = {
    ...ast,
    definitions: ast.definitions.filter(d => d.name.value !== 'Mutation')
  };

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

  const typesFileMetadata = {
    content: typesCodegen,
    name: `hasuraCustomTypes.ts`
  }

  const requiredTypesToImport = { 'Scalars': true, 'Maybe': true };

  const getHandlerFileContent = () => {
    const argumentType = {
      name: camelize(`Action_${mutationName}_input`),
      fields: []
    };
    mutationArguments.forEach(ma => {
      const argTypeMetadata = getWrappingTypeMetadata(ma.type);
      const existingType = typesAst.definitions.find(d => d.name.value === argTypeMetadata.typename)
      if (existingType) {
        if (existingType.kind === 'ScalarTypeDefinition') {
          argumentType.fields.push({
            name: ma.name.value,
            type: getTypescriptTypename(`Scalar['${argTypeMetadata.typename}']`, argTypeMetadata.stack)
          })
        } else {
          argumentType.fields.push({
            name: ma.name.value,
            type: getTypescriptTypename(argTypeMetadata.typename, argTypeMetadata.stack)
          });
          requiredTypesToImport[argTypeMetadata.typename] = true;
        }
      } else { 
        argumentType.fields.push({
          name: ma.name.value,
          type: getTypescriptTypename(`Scalar['${argTypeMetadata.typename}']`, argTypeMetadata.stack)
        })
      }
    });



    const getImports = () => {
      return `
import { Request, Response } from 'express';
import {
${Object.keys(requiredTypesToImport).map(rt => `  ${rt}`).join(',\n')}
} from './hasuraCustomTypes';
`;  
    };

    const getMutationInputType = () => {
      return `
type ${argumentType.name} {
${argumentType.fields.map(f => `  ${f.name}: ${f.type}`).join(',\n')} 
}
`;
    }

    const getHandler = () => {
      return `
const handler = async (request: Request, response: Response) => {
  const mutationInput: ${argumentType.name} = request.body.input;

  // perform your business logic here

  /*

  In case of error,

  return response.status(400).json({
    errors: {
      code: '<error code>',
      message: 'error happened'
    }
  })

  */

  // return the output type
  return response.json({
    data: {}
  });
}
`;

    }

    return `
${getImports()}
${getMutationInputType()}
${getHandler()}

export default handler;
`;

  }

  const handlerFileMetadata = {
    name: `${mutationName}.handler.ts`,
    content: getHandlerFileContent()
  }

  return [typesFileMetadata, handlerFileMetadata];

}

module.exports = templater
