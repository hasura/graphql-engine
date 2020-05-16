import React from 'react';
import { Link } from 'react-router';
import PropTypes from 'prop-types';

import LeftContainer from '../../Common/Layout/LeftContainer/LeftContainer';
import PageContainer from '../../Common/Layout/PageContainer/PageContainer';
import RemoteSchemaSubSidebar from './RemoteSchemaSubSidebar';
import { appPrefix } from './constants';
import { filterItem } from './utils';
import { connect } from 'react-redux';

class RemoteSchemaPageContainer extends React.Component {
  render() {
    const styles = require('../../Common/TableCommon/Table.scss');
    const { children } = this.props;

    const currentLocation = location.pathname;

    const sidebarContent = (
      <ul>
        <li
          role="presentation"
          className={
            currentLocation.includes('remote-schemas/manage')
              ? styles.active
              : ''
          }
        >
          <Link className={styles.linkBorder} to={appPrefix + '/manage'}>
            Manage
          </Link>
          <RemoteSchemaSubSidebar {...this.props} />
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
  }
}

RemoteSchemaPageContainer.propTypes = {
  appPrefix: PropTypes.string.isRequired,
};

const mapStateToProps = state => {
  return {
    ...state,
    dataList: [...state.remoteSchemas.listData.remoteSchemas],
    isError: state.remoteSchemas.listData.isError,
    isRequesting: state.remoteSchemas.listData.isRequesting,
    filtered: [...state.remoteSchemas.listData.filtered],
    searchQuery: state.remoteSchemas.listData.searchQuery,
    viewRemoteSchema: state.remoteSchemas.listData.viewRemoteSchema,
    appPrefix,
  };
};

const mapDispatchToProps = dispatch => {
  return {
    filterItem: filterItem(dispatch),
  };
};

const ConnectedRemoteSchemaPageContainer = connect(
  mapStateToProps,
  mapDispatchToProps
)(RemoteSchemaPageContainer);
export default ConnectedRemoteSchemaPageContainer;
