import { RequestTransformMethod } from '@/metadata/types';
import {
  buildClientSchema,
  getIntrospectionQuery,
  graphqlSync,
  printSchema,
} from 'graphql';
import {
  GraphQLOperationType,
  Oas2,
  Oas3,
  createGraphQLSchema,
} from 'openapi-to-graphql';
import {
  ReferenceObject,
  SchemaObject,
} from 'openapi-to-graphql/dist/types/oas3';
import { Microfiber } from 'microfiber';
import { formatSdl } from 'format-graphql';
import { getActionRequestSampleInput } from '../../../../components/Services/Actions/Add/utils';
import {
  DataDefinition,
  GeneratedAction,
  Operation,
  Result,
  SubDefinition,
} from './types';

const parseRequestMethod = (method: string): RequestTransformMethod => {
  switch (method.toLowerCase()) {
    case 'get':
      return 'GET';
    case 'post':
      return 'POST';
    case 'put':
      return 'PUT';
    case 'delete':
      return 'DELETE';
    case 'patch':
      return 'PATCH';
    default:
      return 'GET';
  }
};

const lastOfArray = (arr: string[]): string => {
  return arr[arr.length - 1];
};

export const formatQuery = (query?: string): string => {
  try {
    return !query || query.trim() === '' ? '' : formatSdl(query);
  } catch (e) {
    return query ?? '';
  }
};

const lowerCaseFirstLetter = (str: string): string => {
  return str ? str.charAt(0).toLowerCase() + str.slice(1) : '';
};

interface Transform {
  transform: Record<string, unknown>;
  needTransform: boolean;
}

const createTransform = (
  definition: SubDefinition,
  prefix: string,
  inverse: boolean
): Transform => {
  try {
    if (Array.isArray(definition)) {
      // union type not supported at the moment
      return { transform: {}, needTransform: false };
    }

    if (
      definition &&
      'preferredName' in definition &&
      typeof definition.preferredName === 'string'
    ) {
      const newPrefix = prefix.split(/\./)[prefix.split(/\./).length - 1];
      const { transform, needTransform } = createTransform(
        definition.subDefinitions,
        newPrefix,
        inverse
      );
      return {
        transform: {
          [`ARRAYSTART(${prefix})`]: true,
          ...transform,
          ARRAYEND: true,
        },
        needTransform,
      };
    }

    let needTransform = false;
    const transform = Object.entries(definition).reduce((acc, curr) => {
      const value = curr[1] as DataDefinition;
      const keyFrom = inverse
        ? lowerCaseFirstLetter(value.preferredName)
        : curr[0];
      const keyTo = inverse
        ? curr[0]
        : lowerCaseFirstLetter(value.preferredName);
      if (keyFrom !== keyTo) {
        needTransform = true;
      }
      if (
        value.schema.type === 'object' ||
        (value.schema.type === 'array' &&
          'subDefinitions' in value.subDefinitions &&
          value.subDefinitions.subDefinitions)
      ) {
        const {
          transform: childrenTransform,
          needTransform: childrenNeedTransform,
        } = createTransform(
          value.subDefinitions,
          `${prefix}?.${keyTo}`,
          inverse
        );
        needTransform = needTransform || childrenNeedTransform;
        return {
          ...acc,
          [keyFrom]: childrenTransform,
        };
      }
      return {
        ...acc,
        [keyFrom]: `{{${prefix}?.${keyTo}}}`,
      };
    }, {});
    return {
      transform,
      needTransform,
    };
  } catch (e) {
    return { transform: {}, needTransform: false };
  }
};

const createSampleInput = (
  definition: SubDefinition
): Record<string, unknown> | Record<string, unknown>[] => {
  try {
    if (Array.isArray(definition)) {
      // union type not supported at the moment
      return {};
    }

    if (
      'preferredName' in definition &&
      typeof definition.preferredName === 'string'
    ) {
      return [
        {
          ...createSampleInput(definition.subDefinitions),
        },
      ];
    }

    return Object.entries(definition).reduce((acc, curr) => {
      const value = curr[1] as DataDefinition;
      const keyFrom = lowerCaseFirstLetter(value.preferredName);
      const keyTo = curr[0];

      if (
        value.schema.type === 'object' ||
        (value.schema.type === 'array' &&
          'subDefinitions' in value.subDefinitions &&
          value.subDefinitions.subDefinitions)
      ) {
        return {
          ...acc,
          // eslint-disable-next-line @typescript-eslint/no-unused-vars
          [keyFrom]: createSampleInput(value.subDefinitions),
        };
      }
      return {
        ...acc,
        [keyFrom]: `${keyTo}`,
      };
    }, {});
  } catch (e) {
    return {};
  }
};

/**
 * transform the generated JSON into a valid kriti template
 */
const postProcessTransform = (output: Transform): string | null => {
  const { transform, needTransform } = output;
  if (!needTransform) {
    return null;
  }
  let string = JSON.stringify(transform, null, 2)
    // removing quotes from values
    .replace(/\"(.*)\": \"(.*)\"/g, '"$1": $2')
    .replace(/\\"/g, '"')
    .replace(/\\n/g, '\n\t')
    // processing arrays
    .replace(
      /\"(.*?)\": \{\n\s*\"ARRAYSTART\((.*)\)\": true,/g,
      '"$1": {{if inverse(empty($2))}} {{ range _, $1 := $2}} {'
    )
    .replace(
      /,(\n\s*?)\"ARRAYEND\": true\n\s*?}/g,
      '$1} {{end}} {{else}} null {{end}}'
    );

  // processing root array
  if (string.match(/\{\n\s*\"ARRAYSTART\((.*)\)\": true,/g)) {
    string = string
      .replace(
        /\{\n\s*\"ARRAYSTART\((.*)\)\": true,/g,
        '{{if inverse(empty($_body))}} {{ range _, item := $_body}} {'
      )
      .replace(/\$body/g, 'item')
      .replace(/\$_body/g, '$body');
  }
  return string;
};

export const createRequestTransform = (operation: Operation): string => {
  let inputName = '';
  const inputObjectType = operation.payloadDefinition?.graphQLInputObjectType;
  if (inputObjectType && 'name' in inputObjectType) {
    inputName = lowerCaseFirstLetter(inputObjectType.name);
  }
  if (operation.payloadDefinition?.subDefinitions) {
    const defaultRequestTransform = ['POST', 'PUT', 'PATCH'].includes(
      operation.method.toUpperCase()
    )
      ? `{{$body.input.${inputName}}}`
      : '';
    return (
      postProcessTransform(
        createTransform(
          operation.payloadDefinition.subDefinitions,
          `$body.input.${inputName}`,
          false
        )
      ) ?? defaultRequestTransform
    );
  }
  return '';
};
export const createResponseTransform = (operation: Operation): string => {
  if (operation.responseDefinition?.targetGraphQLType === 'string') {
    return '{{$body}}';
  }
  if (operation.responseDefinition?.subDefinitions) {
    return (
      postProcessTransform(
        createTransform(
          operation.responseDefinition.subDefinitions,
          '$body',
          true
        )
      ) ?? ''
    );
  }
  return '';
};

export const translateAction = (
  graphqlSchema: Result,
  operation: Operation
): GeneratedAction => {
  const { schema } = graphqlSchema;

  const introspectionQuery = graphqlSync({
    schema,
    source: getIntrospectionQuery(),
  });

  if (introspectionQuery.errors) {
    const joinedMessage = introspectionQuery.errors
      .map(e => e.message)
      .join('\n');
    throw new Error(joinedMessage);
  }

  const microfiber = new Microfiber(introspectionQuery, {
    cleanupSchemaImmediately: false,
  });
  const queryNamesToRemove = (
    microfiber.getQueryType().fields as { name: string; description: string }[]
  )
    ?.filter(
      field =>
        !field.description
          .trim()
          .endsWith(lastOfArray(operation.description.trim().split('\n\n')))
    )
    ?.map(field => field.name);

  const mutationNamesToRemove = (
    microfiber.getMutationType().fields as {
      name: string;
      description: string;
    }[]
  )
    ?.filter(
      field =>
        !field.description
          .trim()
          .endsWith(lastOfArray(operation.description.trim().split('\n\n')))
    )
    ?.map(field => field.name);

  queryNamesToRemove?.forEach(query => {
    microfiber.removeQuery({ name: query, cleanup: false });
  });

  mutationNamesToRemove?.forEach(mutation => {
    microfiber.removeMutation({ name: mutation, cleanup: false });
  });

  microfiber.cleanSchema();

  const newSchema = buildClientSchema(microfiber.getResponse().data);

  const sdl = printSchema(newSchema);

  const sdlWithoutComments = sdl.replace(/"""[^]*?"""/g, '');
  // extratct type query from sdl;
  const typeQuery = sdlWithoutComments.match(/type Query {[^]*?}/g)?.[0];
  const typeMutation = sdlWithoutComments.match(/type Mutation {[^]*?}/g)?.[0];
  const action = formatQuery(typeQuery || typeMutation) ?? '';

  // remove type query and type mutation from sdl
  const sdlWithoutTypeQuery = formatQuery(
    sdlWithoutComments
      .replace(/"""[^]*?"""/g, '')
      .replace(/type Query {[^]*?}/g, '')
      .replace(/type Mutation {[^]*?}/g, '')
      .replace(/type Query\s+/, '')
      .replace(/type Mutation\s+/, '')
  );

  let sampleInput = JSON.parse(
    getActionRequestSampleInput(action, sdlWithoutTypeQuery)
  );

  if (operation.payloadDefinition?.subDefinitions) {
    sampleInput = {
      ...sampleInput,
      input: Object.keys(sampleInput.input).reduce((acc, curr) => {
        return {
          ...acc,
          [curr]:
            curr.toLowerCase() ===
            operation.payloadDefinition?.graphQLTypeName.toLowerCase()
              ? createSampleInput(
                  operation.payloadDefinition?.subDefinitions ?? {}
                )
              : sampleInput.input[curr],
        };
      }, {}),
    };
  }

  const headers =
    operation.parameters
      ?.filter(param => param.in === 'header')
      ?.map(param => param.name) || [];

  const queryParams =
    operation.parameters
      ?.filter(param => param.in === 'query')
      ?.map(param => param.name) || [];
  return {
    operationId: operation.operationId,
    actionType:
      operation.operationType === GraphQLOperationType.Query
        ? 'query'
        : 'mutation',
    action,
    types: sdlWithoutTypeQuery ?? '',
    description: operation?.operation?.description ?? '',
    method: parseRequestMethod(operation?.method),
    baseUrl: graphqlSchema.data?.oass?.[0]?.servers?.[0]?.url ?? '',
    // replace the regex /\{([^}]+)\}/g occurrences with {{$body.input.$1}} with the first letter lowercased
    // e.g. /user/{UserId} -> /user/{{$body.input.userId}}
    path: operation.path.replace(
      /\{([^}]+)\}/g,
      (_, p1) => `{{$body.input.${p1[0].toLowerCase()}${p1.slice(1)}}}`
    ),
    requestTransforms: createRequestTransform(operation) ?? '',
    responseTransforms: createResponseTransform(operation) ?? '',
    sampleInput: JSON.stringify(sampleInput, null, 2),
    headers,
    queryParams,
  };
};

const applyWorkarounds = (properties: (SchemaObject | ReferenceObject)[]) => {
  // eslint-disable-next-line no-restricted-syntax
  for (const property of Object.values(properties ?? {})) {
    if (!('$ref' in property)) {
      // fix boolean enum issue
      if (property.type === 'boolean') {
        delete property.enum;
      }
      if (
        property.type === 'string' &&
        (property.enum || []).some(v => v === 'true' || v === 'false')
      ) {
        delete property.enum;
      }
      if (
        property.type === 'object' &&
        JSON.stringify(property.properties) === '{}'
      ) {
        // fix empty type issue
        property.type = 'string';
        delete property.properties;
      }
      if (property.properties) {
        applyWorkarounds(Object.values(property.properties));
      }
    }
  }
};

const parseOas = async (oas: Oas2 | Oas3): Promise<Result> => {
  const oasCopy = JSON.parse(JSON.stringify(oas)) as Oas3;
  if (oasCopy.components?.schemas) {
    applyWorkarounds(Object.values(oasCopy.components?.schemas));
  }

  return createGraphQLSchema(oasCopy, {
    fillEmptyResponses: true,
    operationIdFieldNames: true,
    simpleEnumValues: true,
    viewer: false,
  });
};

export const generateAction = async (
  oas: Oas2 | Oas3,
  operationId: string
): Promise<GeneratedAction> => {
  const graphqlSchema = await parseOas(oas);
  const operation = graphqlSchema.data.operations[operationId];
  return translateAction(graphqlSchema, operation);
};

export const getOperations = async (oas: Oas2 | Oas3): Promise<Operation[]> => {
  const graphqlSchema = await parseOas(oas);
  return Object.values(graphqlSchema.data.operations);
};
