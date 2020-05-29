import React from 'react';
import { RemoteRelationshipServer } from '../utils';
import RemoteRelationshipEditor from './RemoteRelEditorWrapper';
import { Table } from '../../../../../Common/utils/pgUtils'
import { Dispatch } from '../../../../../../types'

type Props = {
  relationships: RemoteRelationshipServer[];
  table: Table;
  remoteSchemas: string[];
  reduxDispatch: Dispatch;
};

const RemoteRelationshipList: React.FC<Props> = ({
  relationships,
  table,
  remoteSchemas,
  reduxDispatch,
}) => {
  return (
    <React.Fragment>
      {relationships.map(r => (
        <RemoteRelationshipEditor
          key={r.remote_relationship_name}
          relationship={r}
          table={table}
          remoteSchemas={remoteSchemas}
          reduxDispatch={reduxDispatch}
          isLast={false}
        />
      ))}
      <RemoteRelationshipEditor
        key="add-remote-rel"
        table={table}
        remoteSchemas={remoteSchemas}
        reduxDispatch={reduxDispatch}
        isLast
      />
    </React.Fragment>
  );
};

export default RemoteRelationshipList;
