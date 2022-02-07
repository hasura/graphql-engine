import React, { ReactText } from 'react';
import TableCell from './TableCell';
import { RelationshipSourceType, RelationshipType } from '../types';

const RelationshipSourceCell = ({
  relationship,
  tableName,
  sourceType,
}: {
  tableName: ReactText;
  relationship: RelationshipType;
  sourceType?: RelationshipSourceType;
}) => {
  if (
    sourceType === 'to_remote_schema' ||
    sourceType === 'remote_schema_legacy'
  ) {
    return <TableCell tableName={tableName} cols={[relationship?.name]} />;
  }

  if (sourceType === 'to_source') {
    const columns = Object.keys(
      relationship?.definition?.to_source?.field_mapping ?? {}
    ) as string[];
    return <TableCell tableName={relationship?.name} cols={columns} />;
  }

  // todo fk relation is not part of metadata,
  // we need to think for an optimised method (a hook) while developing this functionality
  // old UI uses run_sql to fetch the FK details
  if (sourceType === 'local_object') {
    return <TableCell tableName={tableName} />;
  }
  return <TableCell tableName={tableName} />;
};

export default RelationshipSourceCell;
