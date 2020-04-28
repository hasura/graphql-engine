import React from 'react';
import { Link as RouterLink } from 'react-router';
import PropTypes from 'prop-types';

import LeftContainer from '../../Common/Layout/LeftContainer/LeftContainer';
import PageContainer from '../../Common/Layout/PageContainer/PageContainer';
import RemoteSchemaSubSidebar from './RemoteSchemaSubSidebar';
import styles from '../../Common/TableCommon/Table.scss';

const RemoteSchemaPageContainer = props => {
  const { appPrefix, children } = props;

  const currentLocation = location.pathname;

  const sidebarContent = (
    <ul>
      <li
        role="presentation"
        className={
          currentLocation.includes('remote-schemas/manage') ? styles.active : ''
        }
      >
        <RouterLink className={styles.linkBorder} to={appPrefix + '/manage'}>
          Manage
        </RouterLink>
        <RemoteSchemaSubSidebar {...props} />
      </li>
    </ul>
  );

  const helmet = 'Remote Schemas | Hasura';

  const leftContainer = <LeftContainer>{sidebarContent}</LeftContainer>;

  return (
    <PageContainer helmet={helmet} leftContainer={leftContainer}>
      {children}
    </PageContainer>
  );
};

RemoteSchemaPageContainer.propTypes = {
  appPrefix: PropTypes.string.isRequired,
};

export default (connect, mapStateToProps, mapDispatchToProps) =>
  connect(mapStateToProps, mapDispatchToProps)(RemoteSchemaPageContainer);
