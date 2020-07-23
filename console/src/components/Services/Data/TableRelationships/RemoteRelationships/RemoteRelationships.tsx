import React from 'react';
import styles from '../../TableModify/ModifyTable.scss';
import { RemoteRelationshipServer } from './utils';
import RemoteRelationshipList from './components/RemoteRelationshipList';
import { fetchRemoteSchemas } from '../../../RemoteSchema/Actions';
import { Table } from '../../../../Common/utils/pgUtils';
import ToolTip from '../../../../Common/Tooltip/Tooltip';
import KnowMoreLink from '../../../../Common/KnowMoreLink/KnowMoreLink';
import { Dispatch } from '../../../../../types';

type Props = {
  relationships: RemoteRelationshipServer[];
  reduxDispatch: Dispatch;
  table: Table;
  remoteSchemas: string[];
};

const RemoteRelationships: React.FC<Props> = ({
  relationships,
  reduxDispatch,
  table,
  remoteSchemas,
}) => {
  React.useEffect(() => {
    reduxDispatch(fetchRemoteSchemas());
  }, []);

  return (
    <>
      <h4 className={styles.subheading_text}>
        Remote Schema Relationships
        <ToolTip message="Relationships to remote schemas" />
        &nbsp;
        <KnowMoreLink href="https://hasura.io/docs/1.0/graphql/manual/schema/remote-relationships/remote-schema-relationships.html" />
      </h4>
      <div className={styles.activeEdit}>
        <RemoteRelationshipList
          relationships={relationships}
          table={table}
          remoteSchemas={remoteSchemas}
          reduxDispatch={reduxDispatch}
        />
      </div>
    </>
  );
};

export default RemoteRelationships;
