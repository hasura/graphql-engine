import { TableColumn } from '../../DataSource';
import { RelationshipType } from '../../RelationshipsTable/types';
import { MetadataDataSource } from '../../../metadata/types';
import { ManualObjectRelationship } from '../../hasura-metadata-types';

const boolOperators = ['_and', '_or', '_not'];
export const getBoolOperators = () => {
  const boolMap = boolOperators.map(boolOperator => ({
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
  return columns?.map(column => {
    return {
      kind: 'column',
      name: column.name,
      meta: { name: column.name, type: column.consoleDataType },
    };
  });
};

export const formatTableRelationships = (
  metadataTables: MetadataDataSource['tables']
) => {
  if (!metadataTables) return [];
  const met = metadataTables.reduce(
    (tally: RelationshipType[], curr: RelationshipType) => {
      const object_relationships = curr.object_relationships;
      if (!object_relationships) return tally;
      const relations = object_relationships
        .map(
          (relationship: {
            using: {
              manual_configuration: {
                remote_table: { dataset: string; name: string };
              };
            };
            name: string;
          }) => {
            if (!relationship?.using) return undefined;
            return {
              kind: 'relationship',
              name: relationship?.name,
              meta: {
                name: relationship?.name,
                type: `${relationship?.using?.manual_configuration?.remote_table?.dataset}_${relationship?.using?.manual_configuration?.remote_table?.name}`,
                isObject: true,
              },
            };
          }
        )
        .filter(Boolean);
      return [...tally, ...relations];
    },
    []
  );
  return met;
};

export interface CreateOperatorsArgs {
  tableName: string;
  existingPermission?: Record<string, any>;
  tableColumns: TableColumn[];
  sourceMetadataTables: MetadataDataSource['tables'] | undefined;
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

  const colNames = data.columns.map(col => col.name);
  const relationships = data.relationships.map(
    (rel: ManualObjectRelationship) => rel.name
  );

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
                })
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
          (relationship: ManualObjectRelationship) => key === relationship.name
        );
        const typeName = rel?.meta?.type?.type;

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
    {}
  );

  return operators;
};
