import React, { Component } from 'react';
import { Connect } from 'react-redux';
import Helmet from 'react-helmet';

import globals from '../../../../Globals';

import styles from '../Settings.scss';
import { ReduxState, ConnectInjectedProps } from '../../../../types';

type AboutState = {
  consoleAssetVersion?: string;
};

class About extends Component<ConnectInjectedProps & StateProps> {
  // had to add this here as the state type is not being read properly if added above.
  state: AboutState = {
    consoleAssetVersion: globals.consoleAssetVersion,
  };

  render() {
    const { consoleAssetVersion } = this.state;

    const { serverVersion } = this.props;

    const spinner = <i className="fa fa-spinner fa-spin" />;

    const getServerVersionSection = () => {
      return (
        <div>
          <b>Current server version: </b>
          <span className={styles.add_mar_left_mid}>
            {serverVersion || spinner}
          </span>
        </div>
      );
    };

    const getConsoleAssetVersionSection = () => {
      return (
        <div>
          <b>Console asset version: </b>
          <span className={styles.add_mar_left_mid}>
            {consoleAssetVersion || 'NA'}
          </span>
        </div>
      );
    };

    return (
      <div
        className={`${styles.clear_fix} ${styles.padd_left} ${styles.padd_top} ${styles.metadata_wrapper} container-fluid`}
      >
        <div className={styles.subHeader}>
          <Helmet title="About | Hasura" />
          <h2 className={styles.headerText}>About</h2>
          <div className={styles.add_mar_top}>{getServerVersionSection()}</div>
          <div className={styles.add_mar_top}>
            {getConsoleAssetVersionSection()}
          </div>
        </div>
      </div>
    );
  }
}

const mapStateToProps = (state: ReduxState) => {
  return {
    dataHeaders: state.tables.dataHeaders,
    serverVersion: state.main.serverVersion,
    source: state.tables.currentDataSource,
    latestStableServerVersion: state.main.latestStableServerVersion,
  };
};

type StateProps = ReturnType<typeof mapStateToProps>;

const aboutConnector = (connect: Connect) => connect(mapStateToProps)(About);

export default aboutConnector;
