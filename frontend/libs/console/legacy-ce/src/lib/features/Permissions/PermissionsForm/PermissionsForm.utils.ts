import { TableColumn } from '@hasura/metadata/data-source';
import { getTableDisplayName } from '@hasura/shared/utils';
import { MetadataTable, Source } from '@hasura/shared/types';

const boolOperators = ['_and', '_or', '_not'];
export const getBoolOperators = () => {
  const boolMap = boolOperators.map((boolOperator) => ({
    name: boolOperator,
    kind: 'boolOperator',
    meta: null,
  }));
  return boolMap;
};

const getExistOperators = () => {
  return ['_exists'];
};

export const formatTableColumns = (columns: TableColumn[]) => {
  if (!columns) return [];
  return columns?.map((column) => {
    return {
      kind: 'column',
      name: column.name,
      meta: { name: column.name, type: column.consoleDataType },
    };
  });
};

export const formatTableRelationships = (metadataTables: MetadataTable[]) => {
  if (!metadataTables) return [];
  const met = metadataTables.reduce(
    (tally, curr) => {
      const object_relationships = curr.object_relationships;
      if (!object_relationships?.length) return tally;

      const relations = object_relationships
        .map((relationship) => {
          const relType =
            'manual_configuration' in relationship?.using
              ? getTableDisplayName(
                  relationship.using.manual_configuration.remote_table,
                  '',
                  '_',
                )
              : '';
          return {
            kind: 'relationship',
            name: relationship?.name,
            meta: {
              name: relationship?.name,
              type: relType,
              isObject: true,
            },
          };
        })
        .filter(Boolean);
      return [...tally, ...relations];
    },
    [] as {
      kind: string;
      name: string;
      meta: {
        name: string;
        type: string;
        isObject: boolean;
      };
    }[],
  );
  return met;
};

export interface CreateOperatorsArgs {
  tableName: string;
  existingPermission?: Record<string, any>;
  tableColumns: TableColumn[];
  sourceMetadataTables: Source['tables'] | undefined;
}

export const createOperatorsObject = ({
  tableName = '',
  existingPermission,
  tableColumns,
  sourceMetadataTables,
}: CreateOperatorsArgs): Record<string, any> => {
  if (!existingPermission) {
    return {};
  }

  const data = {
    boolOperators: boolOperators,
    existOperators: getExistOperators(),
    columns: formatTableColumns(tableColumns),
    relationships: sourceMetadataTables
      ? formatTableRelationships(sourceMetadataTables)
      : [],
  };

  const colNames = data.columns.map((col) => col.name);
  const relationships = data.relationships.map((rel) => rel.name);

  const operators = Object.entries(existingPermission).reduce(
    (_acc, [key, value]) => {
      if (boolOperators.includes(key)) {
        return {
          name: key,
          typeName: key,
          type: 'boolOperator',
          [key]: Array.isArray(value)
            ? value.map((each: Record<string, any>) =>
                createOperatorsObject({
                  tableName,
                  tableColumns,
                  existingPermission: each,
                  sourceMetadataTables,
                }),
              )
            : createOperatorsObject({
                tableName,
                tableColumns,
                existingPermission: value,
                sourceMetadataTables,
              }),
        };
      }
      if (relationships.includes(key)) {
        const rel = data.relationships.find(
          (relationship) => key === relationship.name,
        );
        const typeName = rel?.meta?.type;

        return {
          name: key,
          typeName,
          type: 'relationship',
          [key]: createOperatorsObject({
            tableName,
            existingPermission: value,
            tableColumns,
            sourceMetadataTables,
          }),
        };
      }

      if (colNames.includes(key)) {
        return {
          name: key,
          typeName: key,
          type: 'column',
          columnOperator: createOperatorsObject({
            tableName,
            existingPermission: value,
            tableColumns,
            sourceMetadataTables,
          }),
        };
      }

      return key;
    },
    {},
  );

  return operators;
};
