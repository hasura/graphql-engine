import React from 'react';

import LeftNavBar from '../LeftNavBar/LeftNavBar';
import Helmet from 'react-helmet';

import { Link } from 'react-router';

import PropTypes from 'prop-types';

class LayoutWrapper extends React.Component {
  render() {
    const styles = require('../../Data/TableCommon/Table.scss');

    const { appPrefix, children } = this.props;

    const currentLocation = location.pathname;

    return (
      <div>
        <Helmet title={'Custom Resolvers | Hasura'} />
        <div className={styles.wd20 + ' ' + styles.align_left}>
          <div
            className={styles.pageSidebar + ' col-xs-12 ' + styles.padd_remove}
          >
            <div>
              <ul>
                <li
                  role="presentation"
                  className={
                    currentLocation.includes('remote-schemas/manage') ? styles.active : ''
                  }
                >
                  <Link
                    className={styles.linkBorder}
                    to={appPrefix + '/manage'}
                  >
                    <div className={styles.schemaWrapper}>
                      <div
                        className={styles.schemaSidebarSection}
                        data-test="schema"
                      >
                        Manage
                      </div>
                    </div>
                  </Link>
                  <LeftNavBar {...this.props} />
                </li>
              </ul>
            </div>
          </div>
        </div>
        <div className={styles.wd80}>{children}</div>
      </div>
    );
  }
}

LayoutWrapper.propTypes = {
  appPrefix: PropTypes.string.isRequired,
};

export default (connect, mapStateToProps, mapDispatchToProps) =>
  connect(
    mapStateToProps,
    mapDispatchToProps
  )(LayoutWrapper);
