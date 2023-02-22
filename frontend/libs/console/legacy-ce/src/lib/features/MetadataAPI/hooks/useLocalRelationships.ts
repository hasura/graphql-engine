import {
  DataTarget,
  useTableRelationships,
  TableRelationshipsType,
} from '../../Datasources';
import { ArrayRelationship, ObjectRelationship } from '../../../metadata/types';
import { DbToDbRelationship, DbToRemoteSchemaRelationship } from '../types';
import { useArrayRelationships } from './useArrayRelationships';
import { useObjectRelationships } from './useObjectRelationships';

type BaseTypes = {
  name: string;
  reference: string;
  referenceTable: string;
  target: string;
  targetTable?: string;
  fieldsFrom: string[];
  fieldsTo: string[];
};

type DbToDb = {
  fromType: 'table';
  toType: 'database';
  type: 'Object' | 'Array';
  relationship: DbToDbRelationship;
} & BaseTypes;

type Remote = {
  fromType: 'table';
  toType: 'remote_schema';
  type: 'Remote Schema';
  relationship: DbToRemoteSchemaRelationship;
};

type LocalArray = {
  fromType: 'table';
  toType: 'table';
  type: 'Array';
  relationship: ArrayRelationship;
};

type LocalObject = {
  fromType: 'table';
  toType: 'table';
  type: 'Object';
  relationship: ObjectRelationship;
};

type RowData = (DbToDb | LocalObject | LocalArray | Remote) & BaseTypes;

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
    isSuccess: objRlnsIsSuccess,
    isError: objRlnsIsError,
  } = useObjectRelationships(relDef);
  const {
    data: arrRlns,
    isLoading: arrRlnsIsLoading,
    isSuccess: arrRlnsIsSuccess,
    isError: arrRlnsIsError,
  } = useArrayRelationships(relDef);
  const {
    data: tableRlns,
    isLoading: tableRlnsIsLoading,
    isSuccess: tableRlnsIsSuccess,
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
    isSuccess: objRlnsIsSuccess && arrRlnsIsSuccess && tableRlnsIsSuccess,
    isError: objRlnsIsError || arrRlnsIsError || tableRlnsIsError,
  };
};
