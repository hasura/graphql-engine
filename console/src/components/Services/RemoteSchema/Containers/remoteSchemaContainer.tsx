import React from 'react';
import CommonTabLayout from '../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import { NotFoundError } from '../../../Error/PageNotFound';
import { appPrefix } from '../constants';
import { findRemoteSchema } from '../utils';
import styles from '../RemoteSchema.scss';

const tabInfo = {
  details: {
    display_text: 'Details',
  },
  modify: {
    display_text: 'Modify',
  },
  permissions: {
    display_text: 'Permissions',
  },
};

// TODO : types
const RemoteSchemaContainer = ({
  params: { remoteSchemaName },
  children,
  allRemoteSchemas,
  tabName,
  viewRemoteSchema,
}: any) => {
  React.useEffect(() => {
    viewRemoteSchema(remoteSchemaName);
    return () => {
      viewRemoteSchema('');
    };
  }, [remoteSchemaName]);

  const currentRemoteSchema = findRemoteSchema(
    allRemoteSchemas,
    remoteSchemaName
  );

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

  const childrenWithProps = React.Children.map(children, child =>
    React.cloneElement(child, { currentRemoteSchema })
  );

  return (
    <>
      <CommonTabLayout
        appPrefix={appPrefix}
        currentTab={tabName}
        heading={remoteSchemaName}
        tabsInfo={tabInfo}
        breadCrumbs={breadCrumbs}
        baseUrl={`${appPrefix}/manage/${remoteSchemaName}`}
        showLoader={false}
        testPrefix="remote-schema-container-tabs"
      />
      <div className={styles.add_pad_top}>{childrenWithProps}</div>
    </>
  );
};

export default RemoteSchemaContainer;
