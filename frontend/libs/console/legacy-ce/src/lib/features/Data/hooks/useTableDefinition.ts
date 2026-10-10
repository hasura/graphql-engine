import { Table } from '@hasura/shared/types';
import { useParams, useSearchParams } from 'react-router';

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
type TableDefinition = {
  database: string;
  schema?: string | null;
  operation?: string | null;
  table?: Table;
};

//  CONSTANTS
export const TABLE_DEFINITION_SEARCH_KEY = 'table';
const DATASOURCE_DEFINITION_SEARCH_KEY = 'database';

const invalidTableDefinitionResult: QueryStringParseResult = {
  querystringParseResult: 'error',
  errorType: 'invalidTableDefinition',
};
const invalidDatabaseDefinitionResult: QueryStringParseResult = {
  querystringParseResult: 'error',
  errorType: 'invalidDatabaseDefinition',
};

export const useTableDefinition = (): QueryStringParseResult => {
  const params = useParams();
  const [searchParams] = useSearchParams();

  // if tableDefinition is present in query params;
  // Idea is to use query params for GDC tables
  const database =
    params.source || searchParams.get(DATASOURCE_DEFINITION_SEARCH_KEY);

  if (!database) {
    return invalidDatabaseDefinitionResult;
  }

  const rawTable = searchParams.get(TABLE_DEFINITION_SEARCH_KEY);
  const schema = params.schema || searchParams.get('schema');
  const tableName = params.table;
  const operation = params.operation;

  if (schema && tableName) {
    return {
      querystringParseResult: 'success',
      data: {
        database,
        schema,
        operation,
        table: {
          schema,
          name: tableName,
        },
      },
    };
  }

  if (!rawTable) {
    return {
      querystringParseResult: 'success',
      data: { database, schema, operation },
    };
  }

  try {
    return {
      querystringParseResult: 'success',
      data: { database, schema, operation, table: JSON.parse(rawTable) },
    };
  } catch (error) {
    console.error('Unable to parse the table definition', error);
  }

  return invalidTableDefinitionResult;
};
