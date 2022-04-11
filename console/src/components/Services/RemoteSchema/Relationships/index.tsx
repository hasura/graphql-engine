import React from 'react';
import { Tabs } from '../Common/Tabs';
import { appPrefix } from '../constants';
import styles from '../RemoteSchema.scss';
import { RemoteSchemaRelationRenderer } from './RemoteSchemaRelationRenderer';

type Props = { params: { remoteSchemaName: string } };

const RelationshipsConnector = (props: Props) => {
  const { remoteSchemaName } = props.params;

  const breadCrumbs = [
    {
      title: 'Remote schemas',
      url: appPrefix,
    },
    {
      title: 'Manage',
      url: `${appPrefix}/manage`,
    },
    {
      title: remoteSchemaName,
      url: `${appPrefix}/manage/${remoteSchemaName}/modify`,
    },
    {
      title: 'relationships',
      url: '',
    },
  ];

  return (
    <>
      <Tabs
        appPrefix={appPrefix}
        currentTab="relationships"
        heading={remoteSchemaName}
        breadCrumbs={breadCrumbs}
        baseUrl={`${appPrefix}/manage/${remoteSchemaName}`}
        showLoader={false}
        testPrefix="remote-schema-container-tabs"
      />
      <div className={styles.add_pad_top}>
        <RemoteSchemaRelationRenderer remoteSchemaName={remoteSchemaName} />
      </div>
    </>
  );
};

export default RelationshipsConnector;
