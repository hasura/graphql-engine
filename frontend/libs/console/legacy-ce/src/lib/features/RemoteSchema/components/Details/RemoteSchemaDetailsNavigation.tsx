import React from 'react';
import { Tabs } from '../../../../components/Services/RemoteSchema/Common/Tabs';

interface RemoteSchemaDetailsNavigationProps {
  remoteSchemaName: string;
}
export const RemoteSchemaDetailsNavigation = (
  props: RemoteSchemaDetailsNavigationProps
) => {
  const { remoteSchemaName } = props;

  const breadCrumbs = [
    {
      title: 'Remote schemas',
      url: '/remote-schemas',
    },
    {
      title: 'Manage',
      url: `/remote-schemas/manage`,
    },
  ];

  if (remoteSchemaName) {
    breadCrumbs.push({
      title: remoteSchemaName.trim(),
      url: `/remote-schemas/manage/${remoteSchemaName.trim()}/details`,
    });
    breadCrumbs.push({
      title: 'details',
      url: '',
    });
  }

  return (
    <Tabs
      appPrefix="/remote-schemas"
      currentTab="details"
      heading={remoteSchemaName}
      breadCrumbs={breadCrumbs}
      baseUrl={`/remote-schemas/manage/${remoteSchemaName}`}
      showLoader={false}
      testPrefix="remote-schema-details"
    />
  );
};
