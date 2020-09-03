import React from 'react';
import { Link } from 'react-router';
import PropTypes from 'prop-types';

import LeftContainer from '../../../Common/Layout/LeftContainer/LeftContainer';
import PageContainer from '../../../Common/Layout/PageContainer/PageContainer';
import LeftSidebar from '../Sidebar/LeftSidebar';
import styles from '../../../Common/TableCommon/Table.scss';
import { appPrefix } from '../constants';
import { actionsSelector } from '../../../../metadata/selector';

class Container extends React.Component {
  render() {
    const { children } = this.props;

    const currentLocation = location.pathname;

    const sidebarContent = (
      <ul>
        <li
          role="presentation"
          className={
            currentLocation.includes('actions/manage') ? styles.active : ''
          }
        >
          <Link className={styles.linkBorder} to={appPrefix + '/manage'}>
            Manage
          </Link>
          <LeftSidebar appPrefix={appPrefix} {...this.props} />
        </li>
        <li
          role="presentation"
          className={
            currentLocation.includes('actions/types') ? styles.active : ''
          }
        >
          <Link className={styles.linkBorder} to={appPrefix + '/types'}>
            Custom types
          </Link>
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
  }
}

Container.propTypes = {
  appPrefix: PropTypes.string.isRequired,
};

const mapStateToProps = state => {
  return {
    ...state.actions,
    actions: actionsSelector(state),
    readOnlyMode: state.main.readOnlyMode,
  };
};

export default connect => connect(mapStateToProps)(Container);
