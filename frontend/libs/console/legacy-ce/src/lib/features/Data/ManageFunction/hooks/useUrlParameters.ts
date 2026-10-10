import { TableFunction } from '@hasura/shared/types';
import { useParams, useSearchParams } from 'react-router';

//  TYPES
export type QueryStringParseResult =
  | {
      querystringParseResult: 'success';
      qualifiedFunction: TableFunction | undefined;
      operation?: string;
    }
  | {
      querystringParseResult: 'error';
      errorType: 'invalidFunctionDefinition' | 'invalidDatabaseDefinition';
    };

//  CONSTANTS
const FUNCTION_DEFINITION_SEARCH_KEY = 'function';

const invalidDefinitionResult: QueryStringParseResult = {
  querystringParseResult: 'error',
  errorType: 'invalidFunctionDefinition',
};

export const useFunctionURLParameters = () => {
  const params = useParams();
  const [searchParams] = useSearchParams();

  const database = params.source || searchParams.get('source');

  if (!database) {
    return {
      querystringParseResult: 'error',
      errorType: 'invalidDatabaseDefinition',
    };
  }

  const schema = params.schema || searchParams.get('schema');
  const functionName = params.functionName;
  const operation = params.operation;

  if (schema && functionName) {
    return {
      querystringParseResult: 'success',
      qualifiedFunction: {
        name: functionName,
        schema,
      },
      operation,
    };
  }

  // if tableDefinition is present in query params;
  // Idea is to use query params for GDC tables
  const rawFunction = searchParams.get(FUNCTION_DEFINITION_SEARCH_KEY);

  if (!rawFunction) {
    return invalidDefinitionResult;
  }

  try {
    return {
      querystringParseResult: 'success',
      qualifiedFunction: JSON.parse(rawFunction),
      operation,
    };
  } catch (error) {
    console.error('Unable to parse the function definition', error);
  }

  return invalidDefinitionResult;
};
