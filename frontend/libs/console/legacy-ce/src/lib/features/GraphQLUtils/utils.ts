import { MetadataTable } from '../hasura-metadata-types';
import { WhereClause } from '../DataSource/types';
import { OrderBy } from './types';

export const getFields = ({
  columns,
  configuration,
}: {
  columns: string[];
  configuration?: Record<string, any>;
}): string[] => {
  return columns.map(column => {
    const customColumnName = configuration?.custom_column_names?.[column];

    /**
     * column names are the selection set used in the final GQL query.
     * The actual column name from the table is used. But if a custom column name is already set by the user
     * then, the custom name will assume the priority.
     * Reference: https://hasura.io/docs/latest/api-reference/syntax-defs/#columnconfig
     */

    if (customColumnName) return customColumnName;

    return column;
  });
};

export const getGraphQLColumnName = (
  columnName: string,
  tableCustomization: MetadataTable['configuration']
) => {
  /**
   * If there is a custom name for the column return that or stick to the actual one
   */
  return (
    tableCustomization?.column_config?.[columnName]?.custom_name ?? columnName
  );
};

export const getWhereClauses = ({
  whereClause,
  tableCustomization,
}: {
  whereClause?: WhereClause[];
  tableCustomization?: MetadataTable['configuration'];
}): string => {
  if (!whereClause) return '';

  const expressions = whereClause.map(expression => {
    const [columnName, columnExp] = Object.entries(expression)[0];
    const [operator, value] = Object.entries(columnExp)[0];

    const graphQLCompatibleColumnName = getGraphQLColumnName(
      columnName,
      tableCustomization
    );

    return `${graphQLCompatibleColumnName}: { ${operator}: ${
      typeof value === 'string' ? `"${value}"` : JSON.stringify(value)
    }}`;
  });

  return `where: {${expressions.join(',')}}`;
};

export const getOrderByClauses = ({
  orderByClauses,
  tableCustomization,
}: {
  orderByClauses?: OrderBy[];
  tableCustomization?: MetadataTable['configuration'];
}): string => {
  if (!orderByClauses || !orderByClauses.length) return '';

  const expressions = orderByClauses.map(expression => {
    const graphQLCompatibleColumnName = getGraphQLColumnName(
      expression.column,
      tableCustomization
    );

    return `${graphQLCompatibleColumnName}: ${expression.type}`;
  });

  return `order_by: {${expressions.join(',')}}`;
};

export const getLimitClause = (limit?: number) => {
  if (!limit) return '';

  return `limit: ${limit}`;
};

export const getOffsetClause = (offset?: number) => {
  if (!offset) return '';

  return `offset: ${offset}`;
};

export const getScalarType = (type: any): string => {
  if (type.kind === 'SCALAR') return type.name;

  return getScalarType(type.ofType);
};
