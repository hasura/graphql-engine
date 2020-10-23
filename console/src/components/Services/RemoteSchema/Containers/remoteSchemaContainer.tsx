import React from 'react';
import CommonTabLayout from '../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import tabInfo from './remoteSchemaTabs';
import { NotFoundError } from '../../../Error/PageNotFound';
import { appPrefix } from '../constants';
import { VIEW_REMOTE_SCHEMA } from '../Actions';

import { findRemoteSchema } from '../utils';
import styles from '../RemoteSchema.scss';

const RemoteSchemaContainer = ({
  params: { remoteSchemaName },
  children,
  allRemoteSchemas,
  tabName,
  dispatch,
}) => {
  React.useEffect(() => {
    dispatch({ type: VIEW_REMOTE_SCHEMA, data: remoteSchemaName });
    return () => {
      dispatch({ type: VIEW_REMOTE_SCHEMA, data: '' });
    };
  }, [remoteSchemaName]);

  const currentRemoteSchema = findRemoteSchema(
    allRemoteSchemas,
    remoteSchemaName
  );

  if (!currentRemoteSchema) {
    dispatch({ type: VIEW_REMOTE_SCHEMA, data: '' });
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

  //   className={styles.view_stitch_schema_wrapper + ' ' + styles.addWrapper}
  return (
    <div>
      <CommonTabLayout
        appPrefix={appPrefix}
        currentTab={tabName}
        heading={remoteSchemaName}
        tabsInfo={tabInfo}
        breadCrumbs={breadCrumbs}
        baseUrl={`${appPrefix}/manage/${remoteSchemaName}`}
      />
      <div className={styles.add_pad_top}>{childrenWithProps}</div>
    </div>
  );
};

export default RemoteSchemaContainer;
