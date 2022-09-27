import { MetadataTable, Source } from '@/features/MetadataAPI';
import { OrderBy, WhereClause } from '../../../types';
import {
  getFields,
  getLimitClause,
  getOffsetClause,
  getOrderByClauses,
  getWhereClauses,
} from '../../../utils';
import { getQueryRoot } from '../../getQueryRoot';

export const generateGraphQLSelectQuery = async ({
  defaultQueryRoot,
  columns,
  tableCustomization,
  sourceCustomization,
  options,
  operationName,
}: {
  operationName?: string;
  defaultQueryRoot: string;
  columns: string[];
  tableCustomization: MetadataTable['configuration'];
  sourceCustomization: Source['customization'];
  options?: {
    where?: WhereClause;
    offset?: number;
    limit?: number;
    order_by?: OrderBy[];
  };
}) => {
  const queryRoot = getQueryRoot({
    defaultQueryRoot,
    operation: 'select',
    /**
     * Configuration contains the custom names for the following -
     * 1. Table Name
     * 2. Query roots - select, select_by_pk, select_aggregate
     * 3. Column Names
     *
     * Custom names from metadata are user-provided values and will always assume priority in the final GQL schema
     */
    configuration: tableCustomization,
    sourceCustomization,
  });

  const fields = getFields({
    columns,
    configuration: tableCustomization,
  });

  const whereClauses = getWhereClauses({
    whereClause: options?.where,
    tableCustomization,
  });
  const orderByClauses = getOrderByClauses({
    orderByClauses: options?.order_by,
    tableCustomization,
  });
  const limitClause = getLimitClause(options?.limit);
  const offsetClause = getOffsetClause(options?.offset);

  const clauses = [whereClauses, orderByClauses, limitClause, offsetClause]
    .filter(clause => clause.length)
    .join(',');

  const mergedClauses = clauses.length ? `(${clauses})` : '';

  /**
   * If the source has a GQL namespace set for it, then we query for our `queryRoot` under that namespace
   */
  if (sourceCustomization?.root_fields?.namespace)
    return {
      query: `query ${operationName ?? 'MyQuery'}  {
    ${sourceCustomization.root_fields.namespace} ${mergedClauses} {
      ${queryRoot} {
        ${fields.map(field => `${field} \n`)}
      }
    }
  }`,
      resultPath: `${sourceCustomization.root_fields?.namespace}.${queryRoot}`,
    };

  return {
    query: `query ${operationName ?? 'MyQuery'} {
    ${queryRoot} ${mergedClauses} {
      ${fields.map(field => `${field} \n`)}
    }
  }`,
    resultPath: queryRoot,
  };
};
