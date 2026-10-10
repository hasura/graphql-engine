import { FaFolder, FaTable } from 'react-icons/fa';
import { MetadataTable, RunSQLResponse, Table } from '@hasura/shared/types';
import {
  GetTableColumnsProps,
  IntrospectedTable,
  TableFkRelationships,
  TableRow,
} from '../types';
import { areTablesEqual } from '@hasura/metadata/helpers';
import { exportMetadata, runIntrospectionQuery } from '@hasura/metadata/api';
import {
  getScalarType,
  getTypeName,
  extractTableInfo,
  getTableDisplayName,
} from '@hasura/shared/utils';
import { buildClientSchema } from 'graphql';

export const adaptIntrospectedTables = (
  runSqlResponse: RunSQLResponse,
): IntrospectedTable[] => {
  /* 
    The `slice(1)` on the result is done because the first item of the result is always the columns names from the SQL output.
    It is not required for the final result and should be avoided 
  */
  const adaptedResponse = runSqlResponse?.result
    ?.slice(1)
    .map((row: string[]) => ({
      name: `${row[1]}.${row[0]}`,
      table: {
        name: row[0],
        schema: row[1],
      },
      type: row[2],
    }));

  return adaptedResponse ?? [];
};

export const convertToTreeData = (
  tables: Table[],
  hierarchy: string[],
  name: string,
): any => {
  if (!hierarchy.length) return;

  const key = hierarchy[0];

  function onlyUnique(value: any, index: any, self: string | any[]) {
    return self.indexOf(value) === index;
  }

  const levelValues: string[] = tables.map((t: any) => t[key]).filter(Boolean);

  const uniqueLevelValues = levelValues.filter(onlyUnique);

  return [
    ...uniqueLevelValues.map((levelValue) => {
      const { database, ...rest } = JSON.parse(name);

      const _key = JSON.stringify({
        database,
        table: { ...rest.table, [key]: levelValue },
      });
      const children = convertToTreeData(
        tables.filter((t: any) => t[key] === levelValue),
        hierarchy.slice(1),
        _key,
      );

      if (!children)
        return {
          icon: <FaTable />,
          title: levelValue,
          key: _key,
        };

      return {
        icon: <FaFolder />,
        title: levelValue,
        selectable: false,
        children,
        key: _key,
      };
    }),
  ];
};

export const transformGraphqlResponse = ({
  data,
  tableCustomization,
}: {
  data: Record<string, string>[];
  tableCustomization: MetadataTable['configuration'];
}): TableRow[] => {
  return data.map((row) => {
    const transformedRow = Object.entries(row).reduce((acc, [key, value]) => {
      const columnName =
        Object.entries(tableCustomization?.column_config ?? {}).find(
          ([, columnConfig]) => {
            return columnConfig.custom_name === key;
          },
        )?.[0] ?? key;

      return {
        ...acc,
        [columnName]: value,
      };
    }, {});

    return transformedRow;
  });
};

export function generateForeignKeyLabel(foreignKey: TableFkRelationships) {
  const table = extractTableInfo(foreignKey.to.table);
  const toTableLabel = table
    ? getTableDisplayName(foreignKey.to.table)
    : '<Unknown>';

  return `${foreignKey.from.columns
    .join(',')
    // Replace double quotes with empty string
    .replace(/"/g, '')} → ${toTableLabel}.${foreignKey.to.columns
    .join(',')
    .replace(/"/g, '')}`;
}

export const checkSchemaModification = (sql: string): boolean => {
  const sqlStatements = sql
    .toLowerCase()
    .split(';')
    .map((sqlStr) => sqlStr.trim());

  return sqlStatements.some(
    (statement) =>
      statement.startsWith('create ') ||
      statement.startsWith('alter ') ||
      statement.startsWith('drop '),
  );
};

export async function introspectTableScalarTypes({
  dataSourceName,
  table,
  defaultQueryRoot,
  endpoints,
  fetchJson,
}: GetTableColumnsProps & {
  defaultQueryRoot: string;
}) {
  const { metadata } = await exportMetadata({
    fetchJson,
    url: endpoints.metadata,
  });

  if (!metadata) throw Error('Metadata could not be retrieved');

  const metadataSource = metadata.sources.find(
    (s) => s.name === dataSourceName,
  );

  const metadataTable = metadataSource?.tables.find((t) =>
    areTablesEqual(t.table, table),
  );

  if (!metadataTable) throw Error('No table found in metadata');

  const queryRoot = getTypeName({
    defaultQueryRoot,
    operation: 'select',
    sourceCustomization: metadataSource?.customization,
    configuration: metadataTable.configuration,
  });

  const introspectionResult = await runIntrospectionQuery({
    fetchJson,
    url: endpoints.graphQLUrl,
  });
  const schemaType = introspectionResult.__schema.types.find(
    (t) => t.name === queryRoot,
  );

  const graphQLFields =
    schemaType && 'fields' in schemaType ? schemaType.fields : [];
  const schema = buildClientSchema(introspectionResult);

  const scalarTypes = graphQLFields
    .map(({ name, type }) => {
      try {
        return { name, type: getScalarType(type) };
      } catch {
        return null;
      }
    })
    .filter(Boolean);

  return {
    schema,
    scalarTypes,
    metadataTable,
  };
}
