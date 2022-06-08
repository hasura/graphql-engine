import {
  DataTarget,
  useTableRelationships,
  TableRelationshipsType,
} from '@/features/Datasources';
import { RowData } from '@/features/RelationshipsTable';
import { useArrayRelationships } from './useArrayRelationships';
import { useObjectRelationships } from './useObjectRelationships';

const getRelationshipMap = (
  tableRlns: TableRelationshipsType[],
  fkConstraint?: string
) => {
  const destination = tableRlns.find(tr => {
    return tr.from.column.find(c => c?.replace(/"/g, '') === fkConstraint);
  });
  return destination;
};

export const useLocalRelationships = (relDef: DataTarget) => {
  const {
    data: objRlns,
    isLoading: objRlnsIsLoading,
    isError: objRlnsIsError,
  } = useObjectRelationships(relDef);
  const {
    data: arrRlns,
    isLoading: arrRlnsIsLoading,
    isError: arrRlnsIsError,
  } = useArrayRelationships(relDef);
  const {
    data: tableRlns,
    isLoading: tableRlnsIsLoading,
    isError: tableRlnsIsError,
  } = useTableRelationships({ target: relDef });

  const objRlnsData: RowData[] =
    objRlns?.map(relationship => {
      if (relationship?.using?.foreign_key_constraint_on) {
        const relationshipMap = getRelationshipMap(
          tableRlns ?? [],
          relationship?.using?.foreign_key_constraint_on
        );

        return {
          fromType: 'table',
          toType: 'table',
          name: relationship?.name,
          reference: relDef.database,
          referenceTable: relDef.table,
          target: relDef.database,
          targetTable: relationshipMap?.to?.table ?? '',
          type: 'Object',
          fieldsFrom: relationshipMap?.from?.column ?? [],
          fieldsTo: relationshipMap?.to?.column ?? [],
          relationship,
        };
      }
      let targetTable = relationship?.using?.manual_configuration?.remote_table;
      if (typeof targetTable !== 'string') {
        targetTable = targetTable?.name;
      }

      return {
        fromType: 'table',
        toType: 'table',
        name: relationship?.name,
        reference: relDef.database,
        referenceTable: relDef.table,
        target: relDef.database,
        targetTable,
        type: 'Object',
        fieldsFrom: Object.keys(
          relationship?.using?.manual_configuration?.column_mapping ?? {}
        ),
        fieldsTo: Object.values(
          relationship?.using?.manual_configuration?.column_mapping ?? {}
        ),
        relationship,
      };
    }) || [];

  const arrRlnsData: RowData[] =
    arrRlns?.map(relationship => {
      if (relationship?.using?.foreign_key_constraint_on) {
        const relationshipMap = getRelationshipMap(
          tableRlns ?? [],
          relationship?.using?.foreign_key_constraint_on?.column
        );
        const destination =
          typeof relationship?.using?.foreign_key_constraint_on?.table ===
          'string'
            ? relationship?.using?.foreign_key_constraint_on?.table
            : relationship?.using?.foreign_key_constraint_on?.table?.name;
        return {
          fromType: 'table',
          toType: 'table',
          name: relationship?.name,
          reference: relDef?.database,
          referenceTable: relDef?.table ?? '',
          target: relDef.database,
          targetTable: destination || '',
          type: 'Array',
          fieldsFrom: relationshipMap?.from?.column ?? [],
          fieldsTo: relationshipMap?.to?.column ?? [],
          relationship,
        };
      }
      let targetTable = relationship?.using?.manual_configuration?.remote_table;
      if (typeof targetTable !== 'string') {
        targetTable = targetTable?.name;
      }

      return {
        fromType: 'table',
        toType: 'table',
        name: relationship?.name,
        reference: relDef.database,
        referenceTable: relDef.table,
        target: relDef.database,
        targetTable,
        type: 'Array',
        fieldsFrom: Object.keys(
          relationship?.using?.manual_configuration?.column_mapping ?? {}
        ),
        fieldsTo: Object.values(
          relationship?.using?.manual_configuration?.column_mapping ?? {}
        ),
        relationship,
      };
    }) || [];

  return {
    data: [...objRlnsData, ...arrRlnsData],
    isLoading: objRlnsIsLoading || arrRlnsIsLoading || tableRlnsIsLoading,
    isError: objRlnsIsError || arrRlnsIsError || tableRlnsIsError,
  };
};
