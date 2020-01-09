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
  mutationSdl,
  typesSdl
) => {
  const typesAst = parse(`${typesSdl}`);
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

  const mutationAst = parse(`${mutationSdl}`);
  const mutationName = mutationAst.definitions[0].fields[0].name.value;
  const mutationArguments = mutationAst.definitions[0].fields[0].arguments;

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
const handler = (request: Request, response: Response) => {
  const mutationInput: ${argumentType.name} = request.body.input;

  // perform your business logic here

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
