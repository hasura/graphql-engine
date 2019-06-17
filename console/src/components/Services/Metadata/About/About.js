import React, { Component } from 'react';
import styles from './About.scss';

import Endpoints from '../../../../Endpoints';
import Star from '../../../Common/Icons/Star';
import Spinner from '../../../Common/Spinner/Spinner';

import globals from '../../../../Globals';

class About extends Component {
  state = {
    serverVersion: null,
    latestVersionAvailable: null,
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
          latestVersionAvailable: latest.latest,
        })
      );
  }

  render() {
    return (
      <div
        className={`${styles.clear_fix} ${styles.padd_left} ${
          styles.padd_top
        } ${styles.metadata_wrapper} container-fluid`}
      >
        <div className={styles.subHeader}>
          <h2 className={`${styles.heading_text} ${styles.remove_pad_bottom}`}>
            About
          </h2>

          {this.state.serverVersion && this.state.latestVersionAvailable ? (
            <div>
              <div className={styles.add_mar_top + ' ' + styles.wd60}>
                <Star /> <b>Console asset version: </b>
                <span className={styles.add_mar_left_mid}>
                  {this.state.consoleAssetVersion}
                </span>
              </div>
              <hr />
              <div className={styles.add_mar_top + ' ' + styles.wd60}>
                <Star /> <b>Server version: </b>
                <span className={styles.add_mar_left_mid}>
                  {this.state.serverVersion}
                </span>
              </div>
              <hr />
              <div className={styles.add_mar_top + ' ' + styles.wd60}>
                <Star /> <b>Latest available server version: </b>
                <span className={styles.add_mar_left_mid}>
                  {this.state.latestVersionAvailable}
                </span>
              </div>
              <hr />
              {this.state.serverVersion !==
              this.state.latestVersionAvailable ? (
                <div>
                  <a
                    href={
                      'https://github.com/hasura/graphql-engine/releases/tag/' +
                      this.state.latestVersionAvailable
                    }
                    target="_blank"
                    rel="noopener noreferrer"
                  >
                    <span>
                      <i>View Changelog</i>
                    </span>
                  </a>
                  <span className={styles.middot}>
                    {' '}
                    <b>&middot;</b>{' '}
                  </span>
                  <a
                    className={styles.updateLink}
                    href="https://docs.hasura.io/1.0/graphql/manual/deployment/updating.html"
                    target="_blank"
                    rel="noopener noreferrer"
                  >
                    <span>
                      <i>Update Now</i>
                    </span>
                  </a>
                </div>
              ) : (
                ''
              )}
            </div>
          ) : (
            <Spinner />
          )}
        </div>
      </div>
    );
  }
}

const mapStateToProps = state => {
  return {
    //
  };
};

const aboutConnector = connect => connect(mapStateToProps)(About);

export default aboutConnector;
