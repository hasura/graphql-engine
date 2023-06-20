import { formatSdl } from 'format-graphql';
import { Query, RestEndpoint } from '../../hasura-metadata-types';
import { Generator } from './useRestEndpointDefinitions';

type GraphQLType = {
  kind: string;
  name?: string;
  ofType?: GraphQLType;
};

const wrapRoot = (root: string, operation: string) => {
  return root ? `${root} { ${operation} }` : operation;
};

const extractFields = (operation: any, microfiber: any) => {
  const type = microfiber.getType(recursiveType(operation.type));
  const fields = type?.fields
    ?.filter((field: any) => recursiveType(field.type)?.kind === 'SCALAR')
    ?.map((f: { name: string }) => f.name);
  return { fields };
};

export const recursiveType = (type?: GraphQLType): GraphQLType | undefined => {
  if (!type) {
    return undefined;
  }
  if (['OBJECT', 'SCALAR', 'INPUT_OBJECT'].includes(type.kind)) {
    return type;
  } else {
    return recursiveType(type.ofType);
  }
};
export const generateViewEndpoint: Generator['generator'] = (
  root,
  table,
  operation,
  microfiber
) => {
  const { fields } = extractFields(operation, microfiber);

  const idType = recursiveType(
    operation.args?.find((arg: any) => arg.name === 'id')?.type
  )?.name;

  const grapqhlOperation = `
    ${operation.name}(id: $id) {
      ${fields?.join('\n')}
    }
  `;

  const query: Query = {
    name: operation.name,
    query: formatSdl(`
      query ${operation.name}($id: ${idType}!) {
          ${wrapRoot(root, grapqhlOperation)}
      }`),
  };

  const restEndpoint: RestEndpoint = {
    name: operation.name,
    url: `${table}/:id`,
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
  microfiber
) => {
  const { fields } = extractFields(operation, microfiber);

  const grapqhlOperation = `
    ${operation.name} {
      ${fields?.join('\n')}
    }
  `;

  const query: Query = {
    name: operation.name,
    query: formatSdl(`
      query ${operation.name} {
        ${wrapRoot(root, grapqhlOperation)}
      }`),
  };

  const restEndpoint: RestEndpoint = {
    name: operation.name,
    url: `${table}`,
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
  microfiber
) => {
  const { fields } = extractFields(operation, microfiber);
  const idType = recursiveType(
    operation.args?.find((arg: any) => arg.name === 'id')?.type
  )?.name;

  const grapqhlOperation = `
    ${operation.name}(id: $id) {
      ${fields?.join('\n')}
    }
  `;

  const query: Query = {
    name: operation.name,
    query: formatSdl(`
      mutation ${operation.name}($id: ${idType}!) {
          ${wrapRoot(root, grapqhlOperation)}
      }`),
  };

  const restEndpoint: RestEndpoint = {
    name: operation.name,
    url: `${table}/:id`,
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
  microfiber
) => {
  const { fields } = extractFields(operation, microfiber);
  const inputType = recursiveType(
    operation.args?.find((arg: any) => arg.name === 'object')?.type
  )?.name;

  const grapqhlOperation = `
    ${operation.name}(object: $object) {
      ${fields?.join('\n')}
    }
  `;

  const query: Query = {
    name: operation.name,
    query: formatSdl(`
      mutation ${operation.name}($object: ${inputType}!) {
          ${wrapRoot(root, grapqhlOperation)}
      }`),
  };

  const restEndpoint: RestEndpoint = {
    name: operation.name,
    url: `${table}`,
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
  microfiber
) => {
  const { fields } = extractFields(operation, microfiber);
  const idType = recursiveType(
    operation.args?.find((arg: any) => arg.name === 'pk_columns')?.type
  )?.name;

  const inputType = recursiveType(
    operation.args?.find((arg: any) => arg.name === '_set')?.type
  )?.name;

  const grapqhlOperation = `
    ${operation.name}(pk_columns: $id, _set: $object) {
      ${fields?.join('\n')}
    }
  `;

  const query: Query = {
    name: operation.name,
    query: formatSdl(`
      mutation ${operation.name}($id: ${idType}!, $object: ${inputType}!) {
          ${wrapRoot(root, grapqhlOperation)}
      }`),
  };

  const restEndpoint: RestEndpoint = {
    name: operation.name,
    url: `${table}/:id`,
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
