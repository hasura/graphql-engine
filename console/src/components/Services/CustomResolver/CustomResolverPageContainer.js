import React from 'react';
import { Link } from 'react-router';
import PropTypes from 'prop-types';

import LeftContainer from '../../Common/Layout/LeftContainer/LeftContainer';
import PageContainer from '../../Common/Layout/PageContainer/PageContainer';
import LeftNavBar from './LeftNavBar/LeftNavBar';

class CustomResolverPageContainer extends React.Component {
  render() {
    const styles = require('../../Common/TableCommon/Table.scss');
    const { appPrefix, children } = this.props;

    const currentLocation = location.pathname;

    const leftContent = (
      <ul>
        <li
          role="presentation"
          className={
            currentLocation.includes('remote-schemas/manage')
              ? styles.active
              : ''
          }
        >
          <Link
            className={styles.linkBorder}
            to={appPrefix + '/manage'}
          >
            Manage
          </Link>
          <LeftNavBar {...this.props} />
        </li>
      </ul>
    );

    const helmet = 'Remote Schemas | Hasura';

    const leftContainer = (
      <LeftContainer>
        {leftContent}
      </LeftContainer>
    );

    return (
      <PageContainer helmet={helmet} leftContainer={leftContainer}>
        {children}
      </PageContainer>
    );
  }
}

CustomResolverPageContainer.propTypes = {
  appPrefix: PropTypes.string.isRequired,
};

export default (connect, mapStateToProps, mapDispatchToProps) =>
  connect(
    mapStateToProps,
    mapDispatchToProps
  )(CustomResolverPageContainer);
