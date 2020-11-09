import React from 'react';
import CommonTabLayout from '../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import tabInfo from './remoteSchemaTabs';
import { NotFoundError } from '../../../Error/PageNotFound';
import { appPrefix } from '../constants';
import { findRemoteSchema } from '../utils';
import styles from '../RemoteSchema.scss';

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
    <div>
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
    </div>
  );
};

export default RemoteSchemaContainer;
