import zipObject from 'lodash/zipObject';
import { areTablesEqual } from '../../../../hasura-metadata-api';
import {
  LocalTableArrayRelationship,
  LocalTableObjectRelationship,
  MetadataTable,
  SameTableObjectRelationship,
  Table,
  isArrayFkRelationship,
  isObjectFkRelationship,
} from '../../../../hasura-metadata-types';
import {
  SuggestedRelationship,
  TrackedSuggestedRelationship,
  ARRAY_REL_TYPE,
  OBJECT_REL_TYPE,
} from '../types';
import isEqual from 'lodash/isEqual';
import { PostgresTable } from '../../../../DataSource';
import { isNotNull } from '../../../../../components/Services/Data/Common/tsUtils';

type allowedRelationshipTypes = 'object' | 'array';

export const getForeignKeyRelationships = (
  metadataTable: MetadataTable
): ({
  table: Table;
  type: allowedRelationshipTypes;
} & (
  | LocalTableArrayRelationship
  | LocalTableObjectRelationship
  | SameTableObjectRelationship
))[] => {
  const filteredObjectRelationships = (metadataTable.object_relationships ?? [])
    .filter(isObjectFkRelationship)
    .map(rel => ({
      ...rel,
      table: metadataTable.table,
      type: 'object' as allowedRelationshipTypes,
    }));

  const filteredArrayRelationships = (metadataTable.array_relationships ?? [])
    .filter(isArrayFkRelationship)
    .map(rel => ({
      ...rel,
      table: metadataTable.table,
      type: 'array' as allowedRelationshipTypes,
    }));

  return [...filteredObjectRelationships, ...filteredArrayRelationships];
};

export const getTrackedSuggestedRelationships = ({
  suggestedRelationships, // suggested_rels api result
  fkConstraintRelationships, // metadata values
}: {
  suggestedRelationships: SuggestedRelationship[];
  fkConstraintRelationships: ReturnType<typeof getForeignKeyRelationships>;
}): TrackedSuggestedRelationship[] => {
  const result = fkConstraintRelationships
    .map(rel => {
      if (rel.type === 'object') {
        if (
          typeof rel.using.foreign_key_constraint_on === 'string' ||
          Array.isArray(rel.using.foreign_key_constraint_on)
        ) {
          /*
        This means that the constraint column is on the same table. 
        Find a match in the suggestedRelationships API result that matches fromTable === rel.table and where a constraint originates from it 
        https://hasura.io/docs/latest/api-reference/metadata-api/relationship/#1-using-foreign-key-constraint-on-a-column
      */
          const fkConstraint = rel.using.foreign_key_constraint_on;

          const constraintColumns =
            typeof fkConstraint === 'string' ? [fkConstraint] : fkConstraint;
          const matchingResultFromAPI = suggestedRelationships.find(_rel => {
            return (
              areTablesEqual(_rel.from.table, rel.table) &&
              !!_rel.from.constraint_name &&
              isEqual(_rel.from.columns.sort(), constraintColumns.sort())
            );
          });
          if (!matchingResultFromAPI) return null;

          return {
            name: rel.name,
            fromTable: matchingResultFromAPI.from.table,
            toTable: matchingResultFromAPI.to.table,
            columnMapping: zipObject(
              matchingResultFromAPI.from.columns.sort(),
              matchingResultFromAPI.to.columns.sort()
            ),
            type: OBJECT_REL_TYPE,
          };
        } else {
          /*
        This means that the constraint column is on the other table. 
        Find a match in the suggestedRelationships API result that matches fromTable === rel.using.foreign_key_constraint_on.table 
        and where a constraint originates from it 
        https://hasura.io/docs/latest/api-reference/metadata-api/relationship/#2-using-foreign-key-constraint-on-a-remote-table
      */
          const { table: constraintTable, ...rest } =
            rel.using.foreign_key_constraint_on;
          const contraintColumns =
            'column' in rest ? [rest.column] : rest.columns;

          const matchingResultFromAPI = suggestedRelationships.find(_rel => {
            return (
              areTablesEqual(_rel.from.table, constraintTable) &&
              !!_rel.from.constraint_name &&
              isEqual(_rel.from.columns.sort(), contraintColumns.sort())
            );
          });
          if (!matchingResultFromAPI) return null;

          return {
            name: rel.name,
            fromTable: matchingResultFromAPI.to.table,
            toTable: matchingResultFromAPI.from.table,
            columnMapping: zipObject(
              matchingResultFromAPI.to.columns.sort(),
              matchingResultFromAPI.from.columns.sort()
            ),
            type: OBJECT_REL_TYPE,
          };
        }
      }

      if (
        rel.type === 'array' &&
        !(
          /**
           * Typescript is not able to narrow the union type here for some reason, investigate later.
           */
          (
            typeof rel.using.foreign_key_constraint_on === 'string' ||
            Array.isArray(rel.using.foreign_key_constraint_on)
          )
        )
      ) {
        /**
         *  This means that the constraint is on the "to" table in the API result. Find match where -
         * _rel.to.table === rel.using.foreign_key_constraint_on.table && _rel.to.columns === rel.using.foreign_key_constraint_on.columns
         * https://hasura.io/docs/latest/api-reference/metadata-api/relationship/#1-using-foreign-key-constraint-on-a-column-1
         */
        const { table: constraintTable, ...rest } =
          rel.using.foreign_key_constraint_on;
        const contraintColumns =
          'column' in rest ? [rest.column] : rest.columns;

        const matchingResultFromAPI = suggestedRelationships.find(_rel => {
          return (
            areTablesEqual(_rel.to.table, constraintTable) &&
            !!_rel.to.constraint_name &&
            isEqual(_rel.to.columns.sort(), contraintColumns.sort())
          );
        });

        if (!matchingResultFromAPI) return null;

        return {
          name: rel.name,
          fromTable: matchingResultFromAPI.from.table,
          toTable: matchingResultFromAPI.to.table,
          /**
           * According to the docs, the constraint columns are always on the remote table
           */
          columnMapping: zipObject(
            matchingResultFromAPI.from.columns.sort(),
            contraintColumns.sort()
          ),
          type: ARRAY_REL_TYPE,
        };
      }

      return null;
    })
    .filter(isNotNull);

  return result;
};

/**
 * This function should be used only for postgres/mssql tables
 */
export const filterBySchema = (
  rels: ReturnType<typeof getForeignKeyRelationships>,
  schema: string
) => {
  return rels.filter(rel => {
    const table = rel.table as PostgresTable;
    if ('schema' in table) return schema === table.schema;
    // In case gdc tables are used by accident, just return the record with no filter applied.
    return true;
  });
};
