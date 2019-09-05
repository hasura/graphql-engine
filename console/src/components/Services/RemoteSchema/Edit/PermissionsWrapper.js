import React, { useEffect } from 'react';
import Helmet from 'react-helmet';
import { push } from 'react-router-redux';
import Permissions from '../Permissions/Permissions';
import styles from '../RemoteSchema.scss';
import globals from '../../../../Globals';
import tabInfo from './tabInfo';
import CommonTabLayout from '../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import { setCurrentRemoteSchema } from '../Permissions/Actions';
import { appPrefix, pageTitle } from '../constants';

const prefixUrl = globals.urlPrefix + appPrefix;

const PermissionsWrapper = props => {
  const { remoteSchemaName } = props.params;

  if (!remoteSchemaName) {
    props.dispatch(push(prefixUrl));
  }

  useEffect(() => {
    if (remoteSchemaName) {
      props.dispatch(setCurrentRemoteSchema(remoteSchemaName));
    }
    return () => {
      props.dispatch(setCurrentRemoteSchema(''));
    };
  }, [remoteSchemaName]);

  const currentRemoteSchemaData = props.remoteSchemasList.find(r => r.name === remoteSchemaName);
  const remoteSchemaPermissions = currentRemoteSchemaData.permissions || [];

  const breadCrumbs = [
    {
      title: 'Remote schemas',
      url: appPrefix,
    },
    {
      title: 'Manage',
      url: appPrefix + '/' + 'manage',
    },
  ];
  if (remoteSchemaName) {
    breadCrumbs.push({
      title: remoteSchemaName.trim(),
      url:
        appPrefix +
        '/' +
        'manage' +
        '/' +
        remoteSchemaName.trim() +
        '/' +
        'details',
    });
    breadCrumbs.push({
      title: 'permissions',
      url: '',
    });
  }

  return (
    <div className={styles.addWrapper}>
      <Helmet
        title={`Edit ${pageTitle} - ${remoteSchemaName} - ${pageTitle}s | Hasura`}
      />
      <CommonTabLayout
        appPrefix={appPrefix}
        currentTab="permissions"
        heading={remoteSchemaName}
        tabsInfo={tabInfo}
        breadCrumbs={breadCrumbs}
        baseUrl={`${appPrefix}/manage/${remoteSchemaName}`}
        showLoader={false}
      />
      <div className={`${styles.add_pad_top} ${styles.add_pad_bottom}`}>
        <Permissions
          remoteSchemaName={remoteSchemaName}
          existingPermissions={remoteSchemaPermissions}
          {...props}
        />
      </div>
    </div>
  );
};

const mapStateToProps = state => {
  return {
    remoteSchemasList: state.remoteSchemas.listData.remoteSchemas,
    permissions: state.remoteSchemas.permissions,
    adminHeaders: state.tables.dataHeaders,
    rolesList: state.tables.allRoles
  };
};

export default connect => connect(mapStateToProps)(PermissionsWrapper);
