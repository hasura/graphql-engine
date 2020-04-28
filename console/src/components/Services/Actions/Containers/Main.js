import React from 'react';
import { Link as RouterLink } from 'react-router';
import PropTypes from 'prop-types';

import LeftContainer from '../../../Common/Layout/LeftContainer/LeftContainer';
import PageContainer from '../../../Common/Layout/PageContainer/PageContainer';
import LeftSidebar from '../Sidebar/LeftSidebar';
import { appPrefix } from '../constants';
import styles from '../../../Common/TableCommon/Table.scss';

const Container = props => {
  const { children } = props;

  const currentLocation = location.pathname;

  const sidebarContent = (
    <ul>
      <li
        role="presentation"
        className={
          currentLocation.includes('actions/manage') ? styles.active : ''
        }
      >
        <RouterLink className={styles.linkBorder} to={appPrefix + '/manage'}>
          Manage
        </RouterLink>
        <LeftSidebar appPrefix={appPrefix} {...props} />
      </li>
      <li
        role="presentation"
        className={
          currentLocation.includes('actions/types') ? styles.active : ''
        }
      >
        <RouterLink className={styles.linkBorder} to={appPrefix + '/types'}>
          Custom types
        </RouterLink>
      </li>
    </ul>
  );

  const helmet = 'Actions | Hasura';

  const leftContainer = <LeftContainer>{sidebarContent}</LeftContainer>;

  return (
    <PageContainer helmet={helmet} leftContainer={leftContainer}>
      {children}
    </PageContainer>
  );
};

Container.propTypes = {
  appPrefix: PropTypes.string.isRequired,
};

const mapStateToProps = state => {
  return {
    ...state.actions,
  };
};

export default connect => connect(mapStateToProps)(Container);
