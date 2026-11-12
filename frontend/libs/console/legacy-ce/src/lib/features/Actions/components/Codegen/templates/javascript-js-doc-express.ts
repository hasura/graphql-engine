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

const JSDOC_SCALARS: Record<string, string> = {
  ID: 'number',
  Int: 'number',
  Float: 'number',
  String: 'string',
  Boolean: 'boolean',
};

// e.g. `[Int!]` -> `?Array<number>`
const toJSDocType = (type: TypeNode, nullable = true): string => {
  if (type.kind === Kind.NON_NULL_TYPE) {
    return toJSDocType(type.type, false);
  }
  const jsDocType =
    type.kind === Kind.LIST_TYPE
      ? `Array<${toJSDocType(type.type)}>`
      : (JSDOC_SCALARS[type.name.value] ?? type.name.value);
  return nullable ? `?${jsDocType}` : jsDocType;
};

const toJSDocProperty = ({ name, type }: FieldInfo) =>
  `* @property {${toJSDocType(type)}} ${name}`;

const typeMapToJSDoc = (typeMap: TypeMap) => {
  const scalars = typeMap.scalars
    .map(
      (name) => codeBlock`
        /**
         * @typedef {string} ${name}
         */
      `,
    )
    .join('\n\n');

  const enums = Object.entries(typeMap.enums)
    .map(
      ([name, values]) => codeBlock`
        /**
         * @enum {Object} ${name}
         ${values.map((v) => `* @property {string} ${v}`).join('\n')}
         */
      `,
    )
    .join('\n\n');

  const types = Object.entries(typeMap.types)
    .map(
      ([name, fields]) => codeBlock`
        /**
          * @typedef {Object} ${name}
          ${fields.map(toJSDocProperty).join('\n')}
          */
      `,
    )
    .join('\n\n');

  return [scalars, enums, types].join('\n\n');
};

const generateHandler = (
  actionName: string,
  actionOutput: TypeNode,
  typeMap: TypeMap,
) => {
  const returnStatement = `return ${getSampleOutput(actionOutput, typeMap)}`;

  return codeBlock`
    function ${actionName}Handler(args) {
      ${returnStatement}
    }

    // Request Handler
    app.post('/${actionName}', async (req, res) => {
      // get request input
      const params = req.body.input

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
    app.post('/${actionName}', async (req, res) => {
      // get request input
      const params = req.body.input
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
      name: `${actionName}.js`,
      content: derive?.operation
        ? generateDerivedHandler(
            actionName,
            derive.operation,
            derive.endpoint || DEFAULT_HASURA_ENDPOINT,
          )
        : generateHandler(actionName, actionOutput, typeMap),
    },
    {
      name: `${actionName}Types.js`,
      content: typeMapToJSDoc(typeMap),
    },
  ];
};
