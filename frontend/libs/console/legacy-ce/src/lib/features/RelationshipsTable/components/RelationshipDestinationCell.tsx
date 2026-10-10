import ToRsCell from './ToRsCell';
import TableCell from './TableCell';
import { getRemoteFieldPath } from '../utils';
import { RemoteRelationship } from '@hasura/shared/types';
import { getTableDisplayName } from '@hasura/shared/utils';

const RelationshipDestinationCell = ({
  relationship,
}: {
  relationship: RemoteRelationship;
}) => {
  if (!relationship.definition) {
    return <TableCell tableName={relationship.name} />;
  }

  if ('to_remote_schema' in relationship.definition) {
    const remoteField =
      relationship.definition.to_remote_schema?.remote_field ?? {};
    const remoteFieldPath = getRemoteFieldPath(remoteField);

    return (
      <ToRsCell
        rsName={relationship?.definition?.to_remote_schema.remote_schema}
        leafs={remoteFieldPath}
      />
    );
  }

  if ('to_source' in relationship.definition) {
    const columns = Object.values(
      relationship.definition.to_source?.field_mapping ?? {},
    ) as string[];
    const tableName = getTableDisplayName(
      relationship.definition.to_source?.table,
      '',
      ' / ',
    );
    return <TableCell tableName={tableName} cols={columns} />;
  }

  const remoteField = relationship.definition.remote_field ?? {};
  const remoteFieldPath = getRemoteFieldPath(remoteField);

  return (
    <ToRsCell
      rsName={relationship?.definition?.remote_schema}
      leafs={remoteFieldPath}
    />
  );
};
export default RelationshipDestinationCell;
