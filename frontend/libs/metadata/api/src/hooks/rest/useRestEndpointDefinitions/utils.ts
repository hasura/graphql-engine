import { formatGraphQL } from '@hasura/shared/utils';
import { QueryCollectionQuery, RestEndpoint } from '@hasura/shared/types';
import {
  getNamedType,
  isInputObjectType,
  isObjectType,
  isScalarType,
} from 'graphql';
import { Field, Generator, Operation } from './types';

const wrapRoot = (root: string, operation: string) => {
  return root ? `${root} { ${operation} }` : operation;
};

/**
 * Given an operation , it extract all the scalar fields of the type of the operation.
 * This exlcudes, for example, all the relationship fields
 */
const extractFields = (operation: Operation) => {
  const type = getNamedType(operation.type);
  const fields = isObjectType(type)
    ? Object.values(type.getFields())
        .filter((field) => isScalarType(getNamedType(field.type)))
        .map((f) => f.name)
    : undefined;
  return { fields };
};

/**
 * This function builds parts of the query for a given operation.
 * It process the args and produces as output the fields of the operation,
 * the fields of the query and the URL path
 */
const buildArgs = (fields: readonly Field[]) => {
  const args = fields.map((arg) => ({
    name: arg.name,
    type: getNamedType(arg.type).name,
  }));

  const operationArgs = args.map((arg) => `${arg.name}: $${arg.name}`);

  const queryArgs = args.map((arg) => `$${arg.name}: ${arg.type}!`);

  const path = fields.map((arg) => `:${arg.name}`).join('/');

  return { queryArgs, operationArgs, path };
};

const getArgTypeName = (operation: Operation, argName: string) => {
  const arg = operation.args.find((a) => a.name === argName);
  return arg ? getNamedType(arg.type).name : undefined;
};

export const generateViewEndpoint: Generator['generator'] = (
  root,
  table,
  operation,
  customTableName,
) => {
  const { fields } = extractFields(operation);

  const { queryArgs, operationArgs, path } = buildArgs(operation.args);

  const grapqhlOperation = `
    ${operation.name}(${operationArgs}) {
      ${fields?.join('\n')}
    }
  `;

  const query: QueryCollectionQuery = {
    name: operation.name,
    query: formatGraphQL(`
      query ${operation.name}(${queryArgs}) {
          ${wrapRoot(root, grapqhlOperation)}
      }`),
  };

  const restEndpoint: RestEndpoint = {
    name: operation.name,
    url: `${customTableName ? customTableName : table}/${path}`,
    methods: ['GET'],
    definition: {
      query: {
        query_name: operation.name,
        collection_name: 'allowed-queries',
      },
    },
    comment: '',
  };

  return { query, restEndpoint };
};

export const generateViewAllEndpoint: Generator['generator'] = (
  root,
  table,
  operation,
  customTableName,
) => {
  const { fields } = extractFields(operation);

  const grapqhlOperation = `
    ${operation.name} {
      ${fields?.join('\n')}
    }
  `;

  const query: QueryCollectionQuery = {
    name: operation.name,
    query: formatGraphQL(`
      query ${operation.name} {
        ${wrapRoot(root, grapqhlOperation)}
      }`),
  };

  const restEndpoint: RestEndpoint = {
    name: operation.name,
    url: `${customTableName ? customTableName : table}`,
    methods: ['GET'],
    definition: {
      query: {
        query_name: operation.name,
        collection_name: 'allowed-queries',
      },
    },
    comment: '',
  };

  return { query, restEndpoint };
};

export const generateDeleteEndpoint: Generator['generator'] = (
  root,
  table,
  operation,
  customTableName,
) => {
  const { fields } = extractFields(operation);

  const { queryArgs, operationArgs, path } = buildArgs(operation.args);

  const grapqhlOperation = `
    ${operation.name}(${operationArgs}) {
      ${fields?.join('\n')}
    }
  `;

  const query: QueryCollectionQuery = {
    name: operation.name,
    query: formatGraphQL(`
      mutation ${operation.name}(${queryArgs}) {
          ${wrapRoot(root, grapqhlOperation)}
      }`),
  };

  const restEndpoint: RestEndpoint = {
    name: operation.name,
    url: `${customTableName ? customTableName : table}/${path}`,
    methods: ['DELETE'],
    definition: {
      query: {
        query_name: operation.name,
        collection_name: 'allowed-queries',
      },
    },
    comment: '',
  };

  return { query, restEndpoint };
};

export const generateInsertEndpoint: Generator['generator'] = (
  root,
  table,
  operation,
  customTableName,
) => {
  const { fields } = extractFields(operation);
  const inputType = getArgTypeName(operation, 'object');

  const grapqhlOperation = `
    ${operation.name}(object: $object) {
      ${fields?.join('\n')}
    }
  `;

  const query: QueryCollectionQuery = {
    name: operation.name,
    query: formatGraphQL(`
      mutation ${operation.name}($object: ${inputType}!) {
          ${wrapRoot(root, grapqhlOperation)}
      }`),
  };

  const restEndpoint: RestEndpoint = {
    name: operation.name,
    url: `${customTableName ? customTableName : table}`,
    methods: ['POST'],
    definition: {
      query: {
        query_name: operation.name,
        collection_name: 'allowed-queries',
      },
    },
    comment: '',
  };

  return { query, restEndpoint };
};

export const generateUpdateEndpoint: Generator['generator'] = (
  root,
  table,
  operation,
  customTableName,
) => {
  const { fields } = extractFields(operation);

  const pkArg = operation.args.find(
    (arg) => arg.name === 'pk_columns' || arg.name === 'pkColumns',
  );

  if (!pkArg) {
    throw new Error('pk_columns argument is required');
  }

  const pkName = pkArg.name;
  const pkType = getNamedType(pkArg.type);

  const { queryArgs, operationArgs, path } = buildArgs(
    isInputObjectType(pkType) ? Object.values(pkType.getFields()) : [],
  );

  const inputType = getArgTypeName(operation, '_set');

  const grapqhlOperation = `
    ${operation.name}(${pkName}: {
      ${operationArgs}
    }, _set: $object) {
      ${fields?.join('\n')}
    }
  `;

  const query: QueryCollectionQuery = {
    name: operation.name,
    query: formatGraphQL(`
      mutation ${operation.name}(${queryArgs}, $object: ${inputType}!) {
          ${wrapRoot(root, grapqhlOperation)}
      }`),
  };

  const restEndpoint: RestEndpoint = {
    name: operation.name,
    url: `${customTableName ? customTableName : table}/${path}`,
    methods: ['POST'],
    definition: {
      query: {
        query_name: operation.name,
        collection_name: 'allowed-queries',
      },
    },
    comment: '',
  };

  return { query, restEndpoint };
};
