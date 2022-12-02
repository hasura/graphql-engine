//  TYPES
export type QueryStringParseResult =
  | {
      querystringParseResult: 'success';
      data: TableDefinition;
    }
  | {
      querystringParseResult: 'error';
      errorType: 'invalidTableDefinition' | 'invalidDatabaseDefinition';
    };

// TODO better types once GDC kicks in
type TableDefinition = { database: string; table?: unknown };

//  CONSTANTS
const TABLE_DEFINITION_SEARCH_KEY = 'table';
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
const getTableDefinition = (location: Location): QueryStringParseResult => {
  if (!location.search) return invalidTableDefinitionResult;
  // if tableDefinition is present in query params;
  // Idea is to use query params for GDC tables
  const params = new URLSearchParams(location.search);
  const table = params.get(TABLE_DEFINITION_SEARCH_KEY);
  const database = params.get(DATASOURCE_DEFINITION_SEARCH_KEY);

  if (!database) {
    return invalidDatabaseDefinitionResult;
  }

  if (!table) {
    return { querystringParseResult: 'success', data: { database } };
  }

  try {
    return {
      querystringParseResult: 'success',
      data: { database, table: JSON.parse(table) },
    };
  } catch (error) {
    console.error('Unable to parse the table definition', error);
  }

  return invalidTableDefinitionResult;
};

export const useTableDefinition = (location = window.location) => {
  return getTableDefinition(location);
};
