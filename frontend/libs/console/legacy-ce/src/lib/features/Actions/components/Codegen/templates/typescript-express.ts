import { Kind, TypeNode } from 'graphql';
import { CodegenDerive, CodegenFile } from './types';
import { codeBlock } from './codeBlock';
import {
  DEFAULT_HASURA_ENDPOINT,
  FieldInfo,
  TypeMap,
  buildActionTypes,
  getSampleOutput,
} from './actionTypes';

const TS_SCALARS: Record<string, string> = {
  ID: 'number',
  Int: 'number',
  Float: 'number',
  String: 'string',
  Boolean: 'boolean',
};

// e.g. `[Int!]` -> `Maybe<Array<number>>`
const toTypescriptType = (type: TypeNode, nullable = true): string => {
  if (type.kind === Kind.NON_NULL_TYPE) {
    return toTypescriptType(type.type, false);
  }
  const tsType =
    type.kind === Kind.LIST_TYPE
      ? `Array<${toTypescriptType(type.type)}>`
      : (TS_SCALARS[type.name.value] ?? type.name.value);
  return nullable ? `Maybe<${tsType}>` : tsType;
};

const toTypescriptField = ({ name, type }: FieldInfo) =>
  `${type.kind === Kind.NON_NULL_TYPE ? name : `${name}?`}: ${toTypescriptType(type)}`;

const typeMapToTypescript = (typeMap: TypeMap) => {
  const scalars = typeMap.scalars
    .map((name) => `type ${name} = string`)
    .join('\n\n');

  const enums = Object.entries(typeMap.enums)
    .map(
      ([name, values]) => codeBlock`
        enum ${name} {
          ${values.map((v) => `${v} = '${v}'`).join(',\n')}
        }`,
    )
    .join('\n\n');

  const types = Object.entries(typeMap.types)
    .map(
      ([name, fields]) => codeBlock`
        type ${name} = {
          ${fields.map(toTypescriptField).join('\n')}
        }`,
    )
    .join('\n\n');

  return ['type Maybe<T> = T | null', scalars, enums, types].join('\n\n');
};

const generateHandler = (
  actionName: string,
  actionOutput: TypeNode,
  typeMap: TypeMap,
) => {
  // the handler returns the action output type itself, not a `Maybe` of it
  const returnType = toTypescriptType(actionOutput, false);
  const returnStatement = `return ${getSampleOutput(actionOutput, typeMap)}`;

  return codeBlock`
    import { Request, Response } from 'express'

    function ${actionName}Handler(args: ${actionName}Args): ${returnType} {
      ${returnStatement}
    }

    // Request Handler
    app.post('/${actionName}', async (req: Request, res: Response) => {
      // get request input
      const params: ${actionName}Args = req.body.input

      // run some business logic
      const result = ${actionName}Handler(params)

      /*
      // In case of errors:
      return res.status(400).json({
        message: "error happened"
      })
      */

      // success
      return res.json(result)
    })
  `;
};

const generateDerivedHandler = (
  actionName: string,
  operation: string,
  endpoint: string,
) =>
  codeBlock`
    import { Request, Response } from 'express'
    import fetch from 'node-fetch'
    const HASURA_OPERATION =` +
  ` \`${operation}\`\n\n` +
  codeBlock`
    const execute = async (variables) => {
      const fetchResponse = await fetch('${endpoint}', {
        method: 'POST',
        body: JSON.stringify({
          query: HASURA_OPERATION,
          variables,
        }),
      })
      const data = await fetchResponse.json()
      console.log('DEBUG: ', data)
      return data
    }

    // Request Handler
    app.post('/${actionName}', async (req: Request, res: Response) => {
      // get request input
      const params: ${actionName}Args = req.body.input
      // execute the parent operation in Hasura
      const { data, errors } = await execute(params)
      if (errors) return res.status(400).json(errors[0])
      // run some business logic

      // success
      return res.json(data)
    })
  `;

// actionName: Name of the action
// actionsSdl: GraphQL SDL string that has the action and dependent types
// derive: Whether this action was asked to be derived from a Hasura operation
//         derive.operation contains the operation string
export const templater = (
  actionName: string,
  actionsSdl: string,
  derive: CodegenDerive,
): CodegenFile[] => {
  const { actionOutput, typeMap } = buildActionTypes(actionName, actionsSdl);

  return [
    {
      name: `${actionName}.ts`,
      content: derive?.operation
        ? generateDerivedHandler(
            actionName,
            derive.operation,
            derive.endpoint || DEFAULT_HASURA_ENDPOINT,
          )
        : generateHandler(actionName, actionOutput, typeMap),
    },
    {
      name: `${actionName}Types.ts`,
      content: typeMapToTypescript(typeMap),
    },
  ];
};
