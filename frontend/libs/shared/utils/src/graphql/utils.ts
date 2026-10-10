import {
  MetadataTable,
  Where,
  OrderBy,
  WhereClause,
} from '@hasura/shared/types';
import {
  getNullableType,
  GraphQLType,
  isListType,
  isNonNullType,
  isObjectType,
  isWrappingType,
} from 'graphql';

export const getFields = ({
  columns,
  configuration,
}: {
  columns: string[];
  configuration?: Record<string, any>;
}): string[] => {
  return columns.map((column) => {
    const customColumnName = configuration?.['custom_column_names']?.[column];

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
  tableCustomization: MetadataTable['configuration'],
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
  whereClause: Where;
  tableCustomization?: MetadataTable['configuration'];
}): string => {
  if (!whereClause) return '';

  const whereExp = buildWhere({
    where: whereClause,
    tableCustomization,
  });
  // `buildWhere` already returns a fully-braced expression (e.g. `{ _and: [...] }`
  // or `{ col: { _eq: x } }`), so it must not be wrapped in another `{ ... }`.
  return `where: ${whereExp}`;
};

const buildWhere = ({
  where,
  tableCustomization,
}: {
  where: Where;
  tableCustomization?: MetadataTable['configuration'];
}): string => {
  if ('_and' in where) {
    const clauses = (where._and as WhereClause[]).map((whereClause) =>
      buildWhereClause({ whereClause, tableCustomization }),
    );
    return `{ _and: [${clauses.join(',')}]}`;
  }

  if ('_or' in where) {
    const clauses = (where._or as WhereClause[]).map((whereClause) =>
      buildWhereClause({ whereClause, tableCustomization }),
    );
    return `{ _or: [${clauses.join(',')}]}`;
  }

  return buildWhereClause({ whereClause: where, tableCustomization });
};

const buildWhereClause = ({
  whereClause,
  tableCustomization,
}: {
  whereClause: WhereClause;
  tableCustomization?: MetadataTable['configuration'];
}): string => {
  const clauses = Object.entries(whereClause)
    .map(([columnName, columnExp]) => {
      const [operator, value] = Object.entries(columnExp)[0];

      const graphQLCompatibleColumnName = getGraphQLColumnName(
        columnName,
        tableCustomization,
      );

      return `${graphQLCompatibleColumnName}: { ${operator}: ${
        typeof value === 'string' ? `"${value}"` : JSON.stringify(value)
      }}`;
    })
    .join(',');

  return `{${clauses}}`;
};

export const getOrderByClauses = ({
  orderByClauses,
  tableCustomization,
}: {
  orderByClauses?: OrderBy[];
  tableCustomization?: MetadataTable['configuration'];
}): string => {
  if (!orderByClauses || !orderByClauses.length) return '';

  const expressions = orderByClauses.map((expression) => {
    const graphQLCompatibleColumnName = getGraphQLColumnName(
      expression.column,
      tableCustomization,
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

export const getGraphQLUnderlyingType = (gqlType: GraphQLType) => {
  let result = gqlType;

  const wraps: string[] = [];

  while (isWrappingType(result)) {
    if (isListType(result)) {
      wraps.push('l');
      result = result.ofType;
    }

    if (isNonNullType(result)) {
      wraps.push('n');
      result = result.ofType;
    }
  }

  return {
    wraps,
    type: result,
  };
};

export function isScalarGraphQLType(graphQLType?: GraphQLType): boolean {
  if (!graphQLType) {
    return false;
  }
  const nullableType = getNullableType(graphQLType);
  const isObjectOrArray =
    isListType(nullableType) || isObjectType(nullableType);
  return !isObjectOrArray;
}
