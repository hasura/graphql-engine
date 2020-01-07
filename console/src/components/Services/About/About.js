import React, { Component } from 'react';
import Helmet from 'react-helmet';

import Endpoints, { globalCookiePolicy } from '../../../Endpoints';

import globals from '../../../Globals';

import styles from './About.scss';
import requestAction from '../../../utils/requestAction';
import { showErrorNotification } from '../Common/Notification';
import { getRunSqlQuery } from '../../Common/utils/v1QueryUtils';

class About extends Component {
  state = {
    serverVersion: null,
    latestServerVersion: null,
    consoleAssetVersion: globals.consoleAssetVersion,
    pgVersion: null,
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
    const {
      serverVersion,
      latestServerVersion,
      consoleAssetVersion,
      pgVersion,
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
  };
};

const aboutConnector = connect => connect(mapStateToProps)(About);

export default aboutConnector;
