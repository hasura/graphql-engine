import React, { Component } from 'react';
import Helmet from 'react-helmet';

import Endpoints, { globalCookiePolicy } from '../../../Endpoints';
import globals from '../../../Globals';
import requestAction from '../../../utils/requestAction';
import { showErrorNotification } from '../Common/Notification';
import { getRunSqlQuery } from '../../Common/utils/v1QueryUtils';
import { versionGT } from '../../../helpers/versionUtils';
import { Spinner, Heading, Text, TextLink, Box } from '../../UIKit/atoms';
import styles from './About.scss';

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

    const spinner = <Spinner size="sm" display="inline-block" />;

    const getServerVersionSection = () => {
      return (
        <Box mt="20px">
          <Text fontWeight="bold" mr="15px" display="inline-block">
            Current server version:
          </Text>
          {serverVersion || spinner}
        </Box>
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
          <Box mt="20px">
            <TextLink
              href={
                'https://github.com/hasura/graphql-engine/releases/tag/' +
                latestStableServerVersion
              }
              target="_blank"
              fontStyle="italic"
              hover="underline"
              mx="sm"
            >
              View Changelog
            </TextLink>
            <b>&middot;</b>
            <TextLink
              href="https://hasura.io/docs/1.0/graphql/manual/deployment/updating.html"
              target="_blank"
              fontStyle="italic"
              hover="underline"
              ml="sm"
            >
              Update Now
            </TextLink>
          </Box>
        );
      }

      return (
        <Box mt="20px">
          <Text fontWeight="bold" mr="15px" display="inline-block">
            Latest stable server version:
          </Text>
          {latestStableServerVersion || spinner} {updateLinks}
        </Box>
      );
    };

    const getConsoleAssetVersionSection = () => {
      return (
        <Box mt="20px">
          <Text fontWeight="bold" mr="sm" display="inline-block">
            Console asset version:
          </Text>
          {consoleAssetVersion || 'NA'}
        </Box>
      );
    };

    const getPgVersionSection = () => {
      return (
        <Box mt="20px">
          <Text fontWeight="bold" mr="15px" display="inline-block">
            Postgres version:
          </Text>
          {pgVersion || spinner}
        </Box>
      );
    };

    return (
      <div className={`container-fluid ${styles.full_container}`}>
        <div className={styles.subHeader}>
          <Helmet title={'About | Hasura'} />
          <Heading as="h2" fontSize="h2">
            About
          </Heading>
          <Box width="56%">
            {getServerVersionSection()}
            {getLatestServerVersionSection()}
            {getConsoleAssetVersionSection()}
            {getPgVersionSection()}
          </Box>
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
