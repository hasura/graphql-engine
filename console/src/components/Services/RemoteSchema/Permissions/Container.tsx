import React, { ReactElement } from 'react';
import CommonTabLayout from '../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import { NotFoundError } from '../../../Error/PageNotFound';
import { appPrefix } from '../constants';
import styles from '../RemoteSchema.scss';
import { RSPContainerProps } from './types';

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

const RSPContainer: React.FC<RSPContainerProps> = ({
  params: { remoteSchemaName },
  allRemoteSchemas,
  tabName,
  viewRemoteSchema,
  children,
}) => {
  React.useEffect(() => {
    viewRemoteSchema(remoteSchemaName);
    return () => {
      viewRemoteSchema('');
    };
  }, [remoteSchemaName]);

  const currentRemoteSchema = allRemoteSchemas.find(
    rs => rs.name === remoteSchemaName
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
    React.cloneElement(child as ReactElement<any, any>, { currentRemoteSchema })
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

export default RSPContainer;
