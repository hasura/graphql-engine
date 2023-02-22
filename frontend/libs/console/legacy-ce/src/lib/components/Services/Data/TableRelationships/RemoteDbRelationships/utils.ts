import { isEmpty } from '../../../../Common/utils/jsUtils';
import { RemoteDBRelationship } from '../../../../../metadata/types';

export const getRelColumnsMapping = (fields: Record<string, string>) => {
  const colsMapping = Object.entries(fields).map(([k, v]) => ({
    column: k,
    refColumn: v,
  }));
  colsMapping.push({ column: '', refColumn: '' });
  return colsMapping;
};

export const parseDbToDbRemoteRel = (relationship: RemoteDBRelationship) => {
  return {
    relName: relationship?.name,
    relType: relationship?.definition?.to_source?.relationship_type,
    relSource: relationship?.definition?.to_source?.source,
    relTable: {
      name:
        relationship?.definition?.to_source?.table?.name ??
        relationship?.definition?.to_source?.table,
      schema: relationship?.definition?.to_source?.table?.schema ?? '',
    },
    relColumns: isEmpty(relationship?.definition?.to_source?.field_mapping)
      ? [{ column: '', refColumn: '' }]
      : getRelColumnsMapping(
          relationship?.definition?.to_source?.field_mapping
        ),
    relDriver: '',
  };
};

export const getColumnNameArrayFromHookData = (data: string[][]) => {
  return data.slice(1).map(d => d[3]);
};
