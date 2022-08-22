import React from 'react';
import { RelationshipSourceType, RelationshipType } from '../types';
import ToRsCell from './ToRsCell';
import TableCell from './TableCell';
import { getRemoteFieldPath } from '../utils';

const RelationshipDestinationCell = ({
  relationship,
  sourceType,
}: {
  relationship: RelationshipType;
  sourceType: RelationshipSourceType;
}) => {
  if (sourceType === 'to_remote_schema') {
    const remoteField =
      relationship?.definition?.to_remote_schema?.remote_field ?? {};
    const remoteFieldPath = getRemoteFieldPath(remoteField);

    return (
      <ToRsCell
        rsName={relationship?.definition?.to_remote_schema.remote_schema}
        leafs={remoteFieldPath}
      />
    );
  }

  if (sourceType === 'remote_schema_legacy') {
    const remoteField = relationship?.definition?.remote_field ?? {};
    const remoteFieldPath = getRemoteFieldPath(remoteField);

    return (
      <ToRsCell
        rsName={relationship?.definition?.remote_schema}
        leafs={remoteFieldPath}
      />
    );
  }

  if (sourceType === 'to_source') {
    const columns = Object.values(
      relationship?.definition?.to_source?.field_mapping ?? {}
    ) as string[];
    const tableName =
      relationship?.definition?.to_source?.table?.name ??
      relationship?.definition?.to_source?.table;
    return <TableCell tableName={tableName} cols={columns} />;
  }

  if (sourceType === 'local_object') {
    const columns = [
      relationship?.using?.foreign_key_constraint_on,
    ] as string[];
    return <TableCell tableName={relationship?.name} cols={columns} />;
  }
  // local_array
  const columns = [
    relationship?.using?.foreign_key_constraint_on?.column,
  ] as string[];
  return <TableCell tableName={relationship?.name} cols={columns} />;
};
export default RelationshipDestinationCell;
