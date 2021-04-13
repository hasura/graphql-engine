import { parse } from 'graphql';

import globals from '../../../../Globals';
import { AllowedRESTMethods } from '../../../../metadata/types';
import { VariableState } from './LivePreview/state';

// getCurrentPageHost can be used within all components
// that are showing the REST endpoint that will be generated
export const getCurrentPageHost = () => {
  const pageProtocol = window.location.protocol;
  const urlHost = !globals.dataApiUrl
    ? window.location.host
    : globals.dataApiUrl;
  return globals.dataApiUrl ?? `${pageProtocol}://${urlHost}`;
};

const positionsMap = {
  GET: 1,
  POST: 2,
  PUT: 3,
  PATCH: 4,
  DELETE: 5,
};

const modifyMethodsList = (methods: AllowedRESTMethods[]) =>
  methods.map(method => ({ name: method, position: positionsMap[method] }));

// badgeSort is applied when displaying badges
// so that they come in the same order GET, POST, PUT, PATCH, DELETE
export const badgeSort = (methods: AllowedRESTMethods[]) => {
  const modifiedMethods = modifyMethodsList(methods);
  return modifiedMethods
    .sort((a, b) => a.position - b.position)
    .map(method => method.name);
};

// checkIfSubscription is a method being added to prevent endpoints
// with subscriptions being created. See Issue (#628)
// using `any` here since 'operation' is not defined on the type DefinitionNode
const checkIfSubscription = (queryRootNode: any) => {
  return queryRootNode?.operation === 'subscription';
};

const checkIfAnonymousQuery = (queryRootNode: any) => {
  // It probably doesn't have to be this explicit
  return queryRootNode?.name === undefined;
};

// isQueryValid is a helper to validate a query that's being created
// this helps avoiding to creating endpoints with comments, empty spaces
// and queries that are subscriptions (see above comment for reference)
export const isQueryValid = (query: string) => {
  if (!query.trim()) {
    return false;
  }

  try {
    const parsedAST = parse(query);
    if (!parsedAST) {
      return false;
    }
    // making sure that there's only 1 definition in the query
    // query shouldn't be a subscription - server also throws an error for the same
    // also a check's in place to make sure that the query is named
    if (
      parsedAST?.definitions?.length > 1 ||
      checkIfSubscription(parsedAST.definitions[0]) ||
      checkIfAnonymousQuery(parsedAST.definitions[0])
    ) {
      return false;
    }
    return true;
  } catch {
    return false;
  }
};

const acceptedTypeKind = ['NonNullType', 'NamedType'] as const;
const acceptedGQLTypes = [
  'Int',
  'Boolean',
  'String',
  'ID',
  'Float',
  'UUID',
  'Double',
] as const;
export type VariableData = {
  kind: typeof acceptedTypeKind[number] | 'Unsupported';
  type: typeof acceptedGQLTypes[number];
  name: string;
};

// FIXME: this function can be written in a better way!
const getVariableNameKind = (definition: Record<string, any>) => {
  // Ideally, it shouldn't be an empty string ever
  let kind = definition?.type?.kind ?? '';
  const type =
    kind === 'NonNullType'
      ? definition?.type?.type?.name?.value ?? ''
      : definition?.type?.name?.value ?? '';

  if (!acceptedTypeKind.includes(kind)) {
    // NOTE: This does not work well with REST
    // endpoints at the moment
    kind = 'Unsupported';
  }

  return [kind, type];
};

const makeParsedResult = (
  defintion: Record<string, any>
): VariableData[] | undefined => {
  const variableDefinitions = defintion?.variableDefinitions;
  if (!variableDefinitions || !variableDefinitions?.length) {
    return undefined;
  }

  return variableDefinitions.reduce(
    (acc: VariableData[], val: Record<string, any>) => {
      const name = val?.variable?.name?.value ?? '';
      const [kind, type] = getVariableNameKind(val);

      return [...acc, { name, kind, type }];
    },
    []
  );
};

// parseQueryVariables is a helper to parse all the query
// variables and provide information about them for a given query
export const parseQueryVariables = (query: string) => {
  try {
    const parsedAST = parse(query);
    const queryDefinition = parsedAST?.definitions[0] ?? undefined;
    if (!queryDefinition) {
      return undefined;
    }

    return makeParsedResult(queryDefinition);
  } catch {
    return undefined;
  }
};

// TODO: scan the endpoint, if it has any query params,
// if it does, then we should warn the users that query
// params wouldn't work well

type PathnameType = 'literal' | 'variable';
type EndpointData = {
  type: PathnameType;
  value: string;
};

const checkForQueryVariable = (pathString: string): EndpointData => {
  const colonSplit = pathString.split(':');
  // doesn't contain a query variable
  if (colonSplit.length === 1) {
    return {
      type: 'literal',
      value: pathString,
    };
  }

  return {
    type: 'variable',
    value: colonSplit[1],
  };
};

// parseEndpoints helps parse endpoints
// especially by identifying query variables
export const parseEndpoints = (endpoint: string): EndpointData[] | null => {
  try {
    const slashSplit = endpoint.split('/');
    if (!slashSplit || !slashSplit.length) {
      return null;
    }

    return slashSplit.map(checkForQueryVariable);
  } catch {
    // Ideally this shouldn't be happening, because the validity would've been
    // checked by the server and partially at the console too
    // Also, if null's returned, then just dump everything in the response body
    return null;
  }
};

type MakeApiCallArgs = {
  headers: Record<string, string>;
  url: string;
  method: string;
  body?: string;
};

// this is according to the RFC: https://github.com/hasura/graphql-engine/blob/master/rfcs/rest-endpoints.md
const requestErrorsMap: Record<number, string> = {
  400: 'The request is poorly formed (unexpected request or incorrect query variable type(s))',
  404: 'The endpoint does not exist',
  405: 'The endpoint exists, but for a different HTTP method',
  409: 'The request contains inconsistent data (e.g. overlapping endpoint definitions)',
  500: 'The internal state of the server was determined to be incorrect',
} as const;

// makeAPICall is the function that makes the API call
// when the user clicks on `Run Request` button
export const makeAPICall = ({ headers, url, method, body }: MakeApiCallArgs) =>
  fetch(url, { method, credentials: 'include', headers, body })
    .then(response => {
      if (!response.ok) {
        const errorMessage = requestErrorsMap[response.status];
        if (!errorMessage) {
          throw new Error('There was an error processing your request');
        }
        throw new Error(errorMessage);
      }
      return response.json();
    })
    .catch(err => Promise.reject(err));

export const getStatusFromMessage = (errString: string) =>
  Object.keys(requestErrorsMap)
    .map(status => Number(status))
    .find(status => requestErrorsMap[status] === errString);

export const composeEndpoint = (
  endpointData: EndpointData[] | null,
  variableValues: VariableState[],
  endpoint: string
) => {
  if (!endpointData || !variableValues) {
    return endpoint ?? '';
  }

  return endpointData
    .reduce((acc: string[], val) => {
      if (val.type === 'literal') {
        return [...acc, val.value];
      }
      const endpointInfo = variableValues.find(
        varVal => varVal.name === val.value
      );
      if (!endpointInfo) {
        // FIXME?: may cause some issues if the feature is used incorrectly
        return acc;
      }
      return [...acc, endpointInfo.value];
    }, [])
    .join('/');
};

// supportedNumericTypes should be dataSource agnostic (Universal set of all numeric types)
// the main reason why we're not adding this to the DataSourcesAPI
// is that a query can contain multiple sub-queries belonging to
// data sources of different types, hence the query arguments might
// of the different types as well.
export const supportedNumericTypes = [
  'Int',
  'Float',
  'Decimal',
  'Number',
  'numeric',
  'int',
  'serial',
  'bigint',
  'bigserial',
  'float4',
  'float8',
  'real',
  'integer',
  'smallint',
  'double precision',
  'bit',
  'money',
  'smallmoney',
  'tinyint',
  'decimal',
];
