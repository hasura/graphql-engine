import React from 'react';
import { Analytics, REDACT_EVERYTHING } from '../../../../features/Analytics';
import { Tabs } from '../Common/Tabs';
import { appPrefix } from '../constants';
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
      <Analytics name="RemoteSchemaRelationships" {...REDACT_EVERYTHING}>
        <div className="pt-sm">
          <RemoteSchemaRelationRenderer remoteSchemaName={remoteSchemaName} />
        </div>
      </Analytics>
    </>
  );
};

export default RelationshipsConnector;
