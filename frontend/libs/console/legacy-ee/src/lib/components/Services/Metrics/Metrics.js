import React, { useState, useMemo, useEffect } from 'react';
import { Button } from '@hasura/console-legacy-ce';
import LeftPanel from './LeftPanel';
import RightPanel from './RightPanel';
import globals from '../../../Globals';
import { ApolloProvider } from 'react-apollo';
import { makeApolloClient } from '../../../apollo.config';
import { isClientSet } from '../../Login/utils';
import { loadPATState, loadAdminSecretState } from '../../AppState';
import { getMetricsUrl } from './utils';
import LoginWith from '../../Login/LoginWith';
import {
  refetchMetadata as refetchMetadataAction,
  clearCollaboratorSignInState,
} from '../../Main/Actions';
import { isProloginWithPAT } from '../../../utils/utils';
import { isAdmin as _isAdmin } from './utils';
import extendedGlobals from '../../../Globals';
import styles from './Metrics.module.scss';
/*
 * useClient hook will be called for every change in the accessToken and when it changes,
 * a new apollo client instance is created and returned to the required component
 * We are using useMemo here whose key functionality is to do an expensive operation and memoize it.
 * In this scenario we are creating a new client which is sort of like an expensive operation and that should
 * only happen when the accessToken actually changes
 *
 * This hook also keeps the previous client instance in the state which is sort of used to close the old web
 * connections if there exists any
 * */

const useClient = ({ tokenType, token, metricsFQDN }) => {
  // const accessToken = getAccessToken();
  const metricsEndpoint = getMetricsUrl(metricsFQDN);
  const getApolloClient = () => {
    const headers = {};
    if (tokenType === 'pat') {
      headers.authorization = `pat ${token}`;
    } else if (tokenType === 'Bearer') {
      headers.authorization = `Bearer ${token}`;
    }
    return makeApolloClient(metricsEndpoint, headers);
  };
  const [persistedClient, updateClient] = useState(null);
  const c = useMemo(() => {
    if (persistedClient) {
      const { wsLink } = persistedClient;
      const { subscriptionClient: wsClient } = wsLink;
      const resetWebsocket = () => {
        if (wsClient === null) {
          // Nothing to reset!
          return;
        }
        // Close socket connection which will also unregister subscriptions on the server-side.
        wsClient.close();
      };
      resetWebsocket();
    }
    // Create a new client again and return
    const client = getApolloClient();
    updateClient(client);
    return client;
  }, [token]);
  return c;
};

const Metrics = props => {
  const {
    projectId,
    privileges,
    dispatch,
    accessToken,
    projectConfig,
    metricsFQDN,
    location,
    metadata,
    refetchMetadata,
    loading,
  } = props;
  const pat = loadPATState();
  const isAdmin = _isAdmin(privileges);

  let tokenType = '';
  if (accessToken) {
    tokenType = 'Bearer';
  } else if (pat) {
    tokenType = 'pat';
  }

  if (loading) {
    return <div>Loading ...</div>;
  }

  const client = useClient({
    tokenType,
    token: accessToken || pat || '',
    metricsFQDN,
  }).client;

  /*
   * if access token is not available, redirect or redo auth
   * */
  let isAdminSecretMode = false;
  let proMode = false;

  try {
    isAdminSecretMode = !!(
      extendedGlobals.adminSecret ||
      loadAdminSecretState() ||
      globals.adminSecret
    );
  } catch (e) {
    console.error(e);
  }
  try {
    proMode = globals.pro;
  } catch (e) {
    console.error(e);
  }

  useEffect(() => {
    if (
      isAdmin &&
      metadata?.query_collections === null &&
      metadata?.loading !== true
    ) {
      // if there is no query_collections, this will become undefined, null is from the default client state
      refetchMetadata();
    }
  }, [metadata]);

  const renderMetrics = () => {
    if (globals.consoleMode === 'server') {
      if (!isClientSet()) {
        return (
          <div className={styles.noAccessContainer}>
            Looks like Hasura GraphQL Engine is not configured with
            <code>HASURA_GRAPHQL_PRO_KEY</code>. Please checkout our{' '}
            <a
              href="https://docs.pro.hasura.io"
              target="_blank"
              rel="noopener noreferrer"
            >
              docs
            </a>{' '}
            for more info
          </div>
        );
      }

      if (projectId) {
        return (
          <ApolloProvider client={client}>
            <div className={styles.metricsWrapper}>
              <LeftPanel location={location} />
              <RightPanel
                location={location}
                projectId={projectId}
                dispatch={dispatch}
                refetchMetadata={refetchMetadata}
                projectConfig={projectConfig}
                metadata={metadata}
                privileges={privileges}
              >
                {props.children}
              </RightPanel>
            </div>
          </ApolloProvider>
        );
      }

      if (isAdminSecretMode || !globals.isAdminSecretSet) {
        return (
          <div className={styles.noAccessContainer}>
            Please{' '}
            <LoginWith location={location} shouldRedirectBack>
              click here
            </LoginWith>{' '}
            to enable access to the monitoring data.
          </div>
        );
      }

      return (
        <div className={styles.noAccessContainer}>
          Something went wrong! please refresh the page or reach out to us for
          more info
        </div>
      );
    }

    const logoutClick = () => {
      dispatch(clearCollaboratorSignInState());
    };

    if (
      globals.consoleMode === 'cli' &&
      !isProloginWithPAT() &&
      (isAdminSecretMode || globals.isAdminSecretSet)
    ) {
      return (
        <div className={styles.noAccessContainer}>
          <p>
            Please login with the personal access token (PAT) to enable access
            to the monitoring data. Visit{' '}
            <a href="https://hasura.io/docs/latest/graphql/cloud/api-reference.html#authentication">
              {' '}
              the Docs{' '}
            </a>{' '}
            to learn how to generate PAT.
          </p>
          <br />
          <Button mode="default" onClick={logoutClick}>
            Login with PAT
          </Button>
        </div>
      );
    }

    if (globals.consoleMode === 'cli') {
      if (proMode === false) {
        return (
          <div className={styles.noAccessContainer}>
            Something went wrong! please refresh the page or reach out to us for
            more info
          </div>
        );
      }
      return (
        <ApolloProvider client={client}>
          <div className={styles.metricsWrapper}>
            <LeftPanel location={location} />
            <RightPanel
              location={location}
              projectId={projectId}
              dispatch={dispatch}
              refetchMetadata={refetchMetadata}
              projectConfig={projectConfig}
              metadata={metadata}
              privileges={privileges}
            >
              {props.children}
            </RightPanel>
          </div>
        </ApolloProvider>
      );
    }
  };
  return renderMetrics();
};

const mapStateToProps = (state, ownProps) => {
  const projectConfig = state.metrics.projectConfig;
  const metadata = state.main.metadata;
  const project = state.main.project;
  // Check for metrics fqdn in idToken
  const metricsFQDN = project.metricsFQDN;
  return {
    accessToken: state.main.oAuthResponse.access_token,
    location: ownProps.location,
    projectId: project.id,
    projectConfig,
    metricsFQDN,
    metadata,
    privileges: project.privileges,
    loading: !!project.loading,
  };
};

const mapDispatchToProps = dispatch => ({
  refetchMetadata: () => dispatch(refetchMetadataAction()),
  dispatch,
});
const metricsConnector = connect =>
  connect(mapStateToProps, mapDispatchToProps)(Metrics);
export default metricsConnector;
