import React from 'react';
import { RemoteSchema } from '../../../../metadata/types';
import { NotFoundError } from '../../../Error/PageNotFound';
import { Tabs } from '../Common/Tabs';
import { appPrefix } from '../constants';
import styles from '../RemoteSchema.scss';

export type RSPWrapperProps = {
  params: { remoteSchemaName: string };
  allRemoteSchemas?: RemoteSchema[];
  tabName: string;
  viewRemoteSchema: (data: string) => void;
  permissionRenderer: (currentRemoteSchema: RemoteSchema) => React.ReactNode;
};

const RSPWrapper: React.FC<RSPWrapperProps> = ({
  params: { remoteSchemaName },
  allRemoteSchemas,
  tabName,
  viewRemoteSchema,
  permissionRenderer,
}) => {
  React.useEffect(() => {
    viewRemoteSchema(remoteSchemaName);
    return () => {
      viewRemoteSchema('');
    };
  }, [remoteSchemaName]);

  const currentRemoteSchema =
    allRemoteSchemas &&
    allRemoteSchemas.find(rs => rs.name === remoteSchemaName);

  if (!currentRemoteSchema) {
    viewRemoteSchema('');
    throw new NotFoundError();
  }

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
      title: tabName,
      url: '',
    },
  ];

  return (
    <>
      <Tabs
        appPrefix={appPrefix}
        currentTab={tabName}
        heading={remoteSchemaName}
        breadCrumbs={breadCrumbs}
        baseUrl={`${appPrefix}/manage/${remoteSchemaName}`}
        showLoader={false}
        testPrefix="remote-schema-container-tabs"
      />
      <div className={styles.add_pad_top}>
        {permissionRenderer(currentRemoteSchema)}
      </div>
    </>
  );
};

export default RSPWrapper;
