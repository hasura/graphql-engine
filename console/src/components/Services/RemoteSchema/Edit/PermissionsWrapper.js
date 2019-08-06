import React from 'react';
import Helmet from 'react-helmet';
import { push } from 'react-router-redux';
import Permissions from '../Permissions/Permissions';
import styles from '../RemoteSchema.scss';
import globals from '../../../../Globals';
import tabInfo from './tabInfo';
import CommonTabLayout from '../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import { appPrefix, pageTitle } from '../constants';

const prefixUrl = globals.urlPrefix + appPrefix;

const PermissionsWrapper = props => {
  const { remoteSchemaName } = props.params;

  if (!remoteSchemaName) {
    props.dispatch(push(prefixUrl));
  }

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
        <Permissions {...props} />
      </div>
    </div>
  );
};

const mapStateToProps = () => {
  return {};
};

export default connect => connect(mapStateToProps)(PermissionsWrapper);
