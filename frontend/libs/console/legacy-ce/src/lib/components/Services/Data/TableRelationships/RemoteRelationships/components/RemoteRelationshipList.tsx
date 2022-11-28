import React from 'react';
import { RemoteRelationshipServer } from '../utils';
import RemoteRelationshipEditor from './RemoteRelEditorWrapper';
import { Dispatch } from '../../../../../../types';
import { Table } from '../../../../../../dataSources/types';
import { PGFunction } from '../../../../../../dataSources/services/postgresql/types';

type Props = {
  relationships: RemoteRelationshipServer[];
  table: Table;
  remoteSchemas: string[];
  allFunctions: PGFunction[];
  reduxDispatch: Dispatch;
};

const RemoteRelationshipList: React.FC<Props> = ({
  relationships,
  table,
  remoteSchemas,
  allFunctions,
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
          allFunctions={allFunctions}
          isLast={false}
        />
      ))}
      <RemoteRelationshipEditor
        key="add-remote-rel"
        table={table}
        remoteSchemas={remoteSchemas}
        allFunctions={allFunctions}
        reduxDispatch={reduxDispatch}
        isLast
      />
    </React.Fragment>
  );
};

export default RemoteRelationshipList;
