import React, { Component } from 'react';
import Helmet from 'react-helmet';

import Endpoints, { globalCookiePolicy } from '../../../Endpoints';

import globals from '../../../Globals';

import styles from './About.scss';
import requestAction from '../../../utils/requestAction';
import { showErrorNotification } from '../Common/Notification';
import { getRunSqlQuery } from '../../Common/utils/v1QueryUtils';
import { versionGT } from '../../../helpers/versionUtils';

class About extends Component {
  state = {
    consoleAssetVersion: globals.consoleAssetVersion,
    pgVersion: null,
  };

  componentDidMount() {
    const fetchPgVersion = () => {
      const { dispatch, dataHeaders } = this.props;

      const url = Endpoints.query;
      const options = {
        method: 'POST',
        credentials: globalCookiePolicy,
        headers: dataHeaders,
        body: JSON.stringify(getRunSqlQuery('SELECT version();', false, true)),
      };

      dispatch(requestAction(url, options)).then(
        data => {
          this.setState({
            pgVersion: data.result[1][0],
          });
        },
        error => {
          dispatch(
            showErrorNotification('Failed fetching PG version', null, error)
          );
        }
      );
    };

    fetchPgVersion();
  }

  render() {
    const { consoleAssetVersion, pgVersion } = this.state;

    const { serverVersion, latestStableServerVersion } = this.props;

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

    const getLatestServerVersionSection = () => {
      let updateLinks;
      if (
        serverVersion &&
        latestStableServerVersion &&
        versionGT(latestStableServerVersion, serverVersion)
      ) {
        updateLinks = (
          <span className={styles.add_mar_left_mid}>
            <a
              href={
                'https://github.com/hasura/graphql-engine/releases/tag/' +
                latestStableServerVersion
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
              href="https://hasura.io/docs/1.0/graphql/manual/deployment/updating.html"
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
          <b>Latest stable server version: </b>
          <span className={styles.add_mar_left_mid}>
            {latestStableServerVersion || spinner} {updateLinks}
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

    const getPgVersionSection = () => {
      return (
        <div>
          <b>Postgres version: </b>
          <span className={styles.add_mar_left_mid}>
            {pgVersion || spinner}
          </span>
        </div>
      );
    };

    return (
      <div className={`container-fluid ${styles.full_container}`}>
        <div className={styles.subHeader}>
          <Helmet title={'About | Hasura'} />
          <h2 className={styles.headerText}>About</h2>
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
            <div className={styles.add_mar_top}>{getPgVersionSection()}</div>
          </div>
        </div>
      </div>
    );
  }
}

const mapStateToProps = state => {
  return {
    dataHeaders: state.tables.dataHeaders,
    serverVersion: state.main.serverVersion,
    latestStableServerVersion: state.main.latestStableServerVersion,
  };
};

const aboutConnector = connect => connect(mapStateToProps)(About);

export default aboutConnector;
