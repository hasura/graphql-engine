import PropTypes from 'prop-types';
import React from 'react';
import { Link } from 'react-router';
import { Analytics, REDACT_EVERYTHING } from '../../../features/Analytics';
import LeftContainer from '../../Common/Layout/LeftContainer/LeftContainer';
import PageContainer from '../../Common/Layout/PageContainer/PageContainer';
import styles from '../../Common/TableCommon/Table.module.scss';
import RemoteSchemaSubSidebar from './RemoteSchemaSubSidebar';

class RemoteSchemaPageContainer extends React.Component {
  render() {
    const { appPrefix, children } = this.props;

    const currentLocation = location.pathname;

    const sidebarContent = (
      <Analytics name="RemoteSchemaPageContainer" {...REDACT_EVERYTHING}>
        <ul className="bootstrap-jail">
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
      </Analytics>
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

export default (connect, mapStateToProps, mapDispatchToProps) =>
  connect(mapStateToProps, mapDispatchToProps)(RemoteSchemaPageContainer);
