import React from 'react';
import styles from '../../TableModify/ModifyTable.scss';
import { RemoteRelationshipServer } from './utils';
import RemoteRelationshipList from './components/RemoteRelationshipList';
import ToolTip from '../../../../Common/Tooltip/Tooltip';
import KnowMoreLink from '../../../../Common/KnowMoreLink/KnowMoreLink';
import { Dispatch } from '../../../../../types';
import { Table } from '../../../../../dataSources/types';
import { PGFunction } from '../../../../../dataSources/services/postgresql/types';

type Props = {
  relationships: RemoteRelationshipServer[];
  reduxDispatch: Dispatch;
  table: Table;
  allFunctions: PGFunction[];
  remoteSchemas: string[];
};

const RemoteRelationships: React.FC<Props> = ({
  relationships,
  reduxDispatch,
  table,
  allFunctions,
  remoteSchemas,
}) => {
  return (
    <>
      <h4 className={styles.subheading_text}>
        Remote Schema Relationships
        <ToolTip message="Relationships to remote schemas" />
        &nbsp;
        <KnowMoreLink href="https://hasura.io/docs/latest/graphql/core/schema/remote-relationships/remote-schema-relationships.html" />
      </h4>
      <div className={styles.activeEdit}>
        <RemoteRelationshipList
          relationships={relationships}
          table={table}
          allFunctions={allFunctions}
          remoteSchemas={remoteSchemas}
          reduxDispatch={reduxDispatch}
        />
      </div>
    </>
  );
};

export default RemoteRelationships;
