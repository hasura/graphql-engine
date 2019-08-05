import React, { Component } from 'react';

import Endpoints from '../../../Endpoints';

import globals from '../../../Globals';

import styles from './About.scss';

class About extends Component {
  state = {
    serverVersion: null,
    latestServerVersion: null,
    consoleAssetVersion: globals.consoleAssetVersion,
  };

  componentDidMount() {
    fetch(Endpoints.version)
      .then(response => response.json())
      .then(serverVersion =>
        this.setState({
          serverVersion: serverVersion.version,
        })
      );

    fetch(Endpoints.updateCheck)
      .then(response => response.json())
      .then(latest =>
        this.setState({
          latestServerVersion: latest.latest,
        })
      );
  }

  render() {
    const {
      serverVersion,
      latestServerVersion,
      consoleAssetVersion,
    } = this.state;

    const spinner = <i className="fa fa-spinner fa-spin" />;

    const getServerVersionSection = () => {
      return (
        <div>
          <b>Server version: </b>
          <span className={styles.add_mar_left_mid}>
            {serverVersion || spinner}
          </span>
        </div>
      );
    };

    const getLatestServerVersionSection = () => {
      let updateLinks;
      if (
        serverVersion &&
        latestServerVersion &&
        serverVersion !== latestServerVersion
      ) {
        updateLinks = (
          <span className={styles.add_mar_left_mid}>
            <a
              href={
                'https://github.com/hasura/graphql-engine/releases/tag/' +
                latestServerVersion
              }
              target="_blank"
              rel="noopener noreferrer"
            >
              <span>
                <i>View Changelog</i>
              </span>
            </a>
            <span>
              &nbsp;<b>&middot;</b>&nbsp;
            </span>
            <a
              href="https://docs.hasura.io/1.0/graphql/manual/deployment/updating.html"
              target="_blank"
              rel="noopener noreferrer"
            >
              <span>
                <i>Update Now</i>
              </span>
            </a>
          </span>
        );
      }

      return (
        <div>
          <b>Latest server version: </b>
          <span className={styles.add_mar_left_mid}>
            {latestServerVersion || spinner} {updateLinks}
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
        className={`container-fluid ${styles.add_mar_top}  ${styles.add_mar_left}`}
      >
        <div className={styles.subHeader}>
          <h2 className={`${styles.heading_text} ${styles.remove_pad_bottom}`}>
            About
          </h2>
          <div className={styles.wd60}>
            <div className={styles.add_mar_top}>
              {getServerVersionSection()}
            </div>
            <div className={styles.add_mar_top}>
              {getLatestServerVersionSection()}
            </div>
            <div className={styles.add_mar_top}>
              {getConsoleAssetVersionSection()}
            </div>
          </div>
        </div>
      </div>
    );
  }
}

const mapStateToProps = () => {
  return {};
};

const aboutConnector = connect => connect(mapStateToProps)(About);

export default aboutConnector;
