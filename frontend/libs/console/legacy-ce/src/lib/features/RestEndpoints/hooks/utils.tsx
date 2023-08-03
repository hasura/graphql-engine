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

export type Operation = {
  name: string;
  type: GraphQLType;
  args: Field[];
};

type Field = {
  type: GraphQLType;
  name: string;
};

type MicrofiberType = {
  fields: Field[];
};

/**
 * Given an operation , it extract all the scalar fields of the type of the operation.
 * This exlcudes, for example, all the relationship fields
 */
const extractFields = (operation: Operation, microfiber: any) => {
  const type: MicrofiberType = microfiber.getType(
    recursiveType(operation.type)
  );
  const fields = type?.fields
    ?.filter(field => recursiveType(field.type)?.kind === 'SCALAR')
    ?.map((f: { name: string }) => f.name);
  return { fields };
};

/**
 * This function builds parts of the query for a given operation.
 * It process the args and produces as output the fields of the operation,
 * the fields of the query and the URL path
 */
const buildArgs = (fields: Field[]) => {
  const args = fields?.map(arg => ({
    name: arg.name,
    type: recursiveType(arg.type)?.name,
  }));

  const operationArgs = args?.map(arg => `${arg.name}: $${arg.name}`);

  const queryArgs = args?.map(arg => `$${arg.name}: ${arg.type}!`);

  const path = fields?.map(arg => `:${arg.name}`).join('/');

  return { queryArgs, operationArgs, path };
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

  const { queryArgs, operationArgs, path } = buildArgs(operation.args);

  const grapqhlOperation = `
    ${operation.name}(${operationArgs}) {
      ${fields?.join('\n')}
    }
  `;

  const query: Query = {
    name: operation.name,
    query: formatSdl(`
      query ${operation.name}(${queryArgs}) {
          ${wrapRoot(root, grapqhlOperation)}
      }`),
  };

  const restEndpoint: RestEndpoint = {
    name: operation.name,
    url: `${table}/${path}`,
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

  const { queryArgs, operationArgs, path } = buildArgs(operation.args);

  const grapqhlOperation = `
    ${operation.name}(${operationArgs}) {
      ${fields?.join('\n')}
    }
  `;

  const query: Query = {
    name: operation.name,
    query: formatSdl(`
      mutation ${operation.name}(${queryArgs}) {
          ${wrapRoot(root, grapqhlOperation)}
      }`),
  };

  const restEndpoint: RestEndpoint = {
    name: operation.name,
    url: `${table}/${path}`,
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
    operation.args?.find(arg => arg.name === 'object')?.type
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

  const pkArg = operation.args?.find(
    arg => arg.name === 'pk_columns' || arg.name === 'pkColumns'
  );

  if (!pkArg) {
    throw new Error('pk_columns argument is required');
  }

  const pkTypeName = recursiveType(pkArg.type)?.name;
  const pkName = pkArg.name;

  const pkType = microfiber.getType({
    kind: 'INPUT_OBJECT',
    name: pkTypeName,
  });

  const { queryArgs, operationArgs, path } = buildArgs(pkType?.inputFields);

  const inputType = recursiveType(
    operation.args?.find(arg => arg.name === '_set')?.type
  )?.name;

  const grapqhlOperation = `
    ${operation.name}(${pkName}: {
      ${operationArgs}
    }, _set: $object) {
      ${fields?.join('\n')}
    }
  `;

  const query: Query = {
    name: operation.name,
    query: formatSdl(`
      mutation ${operation.name}(${queryArgs}, $object: ${inputType}!) {
          ${wrapRoot(root, grapqhlOperation)}
      }`),
  };

  const restEndpoint: RestEndpoint = {
    name: operation.name,
    url: `${table}/${path}`,
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
