import React from 'react';
import { LearnMoreLink } from '../../../../../new-components/LearnMoreLink';
import styles from '../../TableModify/ModifyTable.module.scss';
import { RemoteRelationshipServer } from './utils';
import RemoteRelationshipList from './components/RemoteRelationshipList';
import ToolTip from '../../../../Common/Tooltip/Tooltip';
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
        <LearnMoreLink
          href="https://hasura.io/docs/latest/graphql/core/schema/remote-relationships/remote-schema-relationships.html"
          className="font-normal"
        />
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
