import { QualifiedFunction } from '../../../hasura-metadata-types';

//  TYPES
export type QueryStringParseResult =
  | {
      querystringParseResult: 'success';
      data: FunctionDefinition;
    }
  | {
      querystringParseResult: 'error';
      errorType: 'invalidTableDefinition' | 'invalidDatabaseDefinition';
    };

// TODO better types once GDC kicks in
type FunctionDefinition = { database: string; function?: QualifiedFunction };

//  CONSTANTS
const FUNCTION_DEFINITION_SEARCH_KEY = 'function';
const DATASOURCE_DEFINITION_SEARCH_KEY = 'database';

const invalidTableDefinitionResult: QueryStringParseResult = {
  querystringParseResult: 'error',
  errorType: 'invalidTableDefinition',
};
const invalidDatabaseDefinitionResult: QueryStringParseResult = {
  querystringParseResult: 'error',
  errorType: 'invalidDatabaseDefinition',
};

//  FUNCTION
const getFunctionDefinitionFromUrl = (
  location: Location
): QueryStringParseResult => {
  if (!location.search) return invalidTableDefinitionResult;
  // if tableDefinition is present in query params;
  // Idea is to use query params for GDC tables
  const params = new URLSearchParams(location.search);
  const qualifiedFunction = params.get(FUNCTION_DEFINITION_SEARCH_KEY);
  const database = params.get(DATASOURCE_DEFINITION_SEARCH_KEY);

  if (!database) {
    return invalidDatabaseDefinitionResult;
  }

  if (!qualifiedFunction) {
    return { querystringParseResult: 'success', data: { database } };
  }

  try {
    return {
      querystringParseResult: 'success',
      data: { database, function: JSON.parse(qualifiedFunction) },
    };
  } catch (error) {
    console.error('Unable to parse the function definition', error);
  }

  return invalidTableDefinitionResult;
};

export const useURLParameters = (location = window.location) => {
  return getFunctionDefinitionFromUrl(location);
};
