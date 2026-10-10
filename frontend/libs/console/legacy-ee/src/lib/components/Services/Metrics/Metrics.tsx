import React from 'react';
import LeftPanel from './LeftPanel';
import RightPanel from './RightPanel';
import { ApolloProvider } from '@apollo/client/react';
import {
  type ApolloClientOutput,
  disposeApolloClient,
  makeApolloClient,
} from '../../../apollo.config';
import { getMetricsUrl } from './utils';
import LoginWith from '../../Login/LoginWithHasura';
import styles from './Metrics.module.scss';
import prometheusMonitoring from './images/prometheus_monitoring.svg';
import { useAuthContext } from '../../../shared/auth/context';
import { useProjectInfo } from '../../../hooks/useProjectInfo';
import { Outlet } from 'react-router';
import { Button, Link, Text } from '@hasura/shared/ui';
import { Code, Skeleton } from '@radix-ui/themes';
import { useAppContext } from '@hasura/shared/context';

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

const Metrics = () => {
  const { envVars } = useAppContext();
  const { data: projectInfo, isFetching: projectInfoLoading } =
    useProjectInfo();
  const { logout, authType, getMetricsHeaders } = useAuthContext();
  const [persistedClient, updateClient] =
    React.useState<ApolloClientOutput | null>();

  React.useEffect(() => {
    if (!projectInfo) {
      return;
    }

    if (persistedClient) {
      disposeApolloClient(persistedClient);
    }

    let client: ApolloClientOutput | undefined;

    (authType === 'hasura-sso' || authType === 'pat'
      ? getMetricsHeaders()
      : Promise.resolve({})
    ).then((metricsHeaders) => {
      const metricsEndpoint = getMetricsUrl(projectInfo.metricsFQDN);
      // Create a new client again and return
      client = makeApolloClient(metricsEndpoint, metricsHeaders);
      updateClient(client);
    });

    return () => {
      try {
        if (client) {
          disposeApolloClient(client);
        }
      } catch {
        // Teardown-only: the Apollo client may already be disposed on unmount;
        // there is nothing actionable to recover here.
      }
    };
  }, [projectInfoLoading, projectInfo, authType, getMetricsHeaders]);

  if (projectInfoLoading) {
    return (
      <div className="p-6">
        <Skeleton width="100%" height="20px" />
      </div>
    );
  }

  /*
   * if access token is not available, redirect or redo auth
   * */
  const proMode =
    envVars.consoleType === 'pro' ||
    envVars.consoleType === 'cloud' ||
    (envVars.consoleType === 'pro-lite' && envVars.projectID);

  const renderMetrics = () => {
    if (envVars.consoleMode === 'server') {
      if (!envVars.projectID) {
        return (
          <div className="p-6">
            <Text>
              Looks like Hasura GraphQL Engine is not configured with
              <Code>HASURA_GRAPHQL_PRO_KEY</Code>. Please checkout our{' '}
              <Link
                href="https://docs.pro.hasura.io"
                target="_blank"
                rel="noopener noreferrer"
              >
                docs
              </Link>{' '}
              for more info
            </Text>
          </div>
        );
      }

      const projectId = projectInfo?.id || envVars.projectID;

      if (persistedClient) {
        const metricsContent = (
          <ApolloProvider client={persistedClient.client}>
            <div className={styles['metricsWrapper']}>
              <LeftPanel />
              <RightPanel>
                <Outlet />
              </RightPanel>
            </div>
          </ApolloProvider>
        );

        return (
          <>
            <a
              href={`${window.location.protocol}//${window.location.host}/project/${projectId}/monitoring`}
              target="_blank"
              className={styles['banner']}
              rel="noreferrer"
            >
              <img
                src={prometheusMonitoring}
                alt="Monitoring Icon"
                className={styles['bannerIcon']}
              />
              <div className={styles['bannerText']}>
                Access more metrics with our new dashboard!
              </div>
            </a>
            {metricsContent}
          </>
        );
      }

      if (authType === 'admin-secret' || authType === 'sso') {
        return (
          <div className="p-6">
            <Text>
              Please <LoginWith shouldRedirectBack>click here</LoginWith> to
              enable access to the monitoring data.
            </Text>
          </div>
        );
      }

      return <ErrorText />;
    }

    const logoutClick = () => {
      logout();
    };

    if (envVars.consoleMode === 'cli' && authType !== 'pat') {
      return (
        <div className="p-6">
          <Text>
            Please login with the personal access token (PAT) to enable access
            to the monitoring data. Visit{' '}
            <Link href="https://hasura.io/docs/latest/graphql/cloud/api-reference.html#authentication">
              {' '}
              the Docs{' '}
            </Link>{' '}
            to learn how to generate PAT.
          </Text>
          <br />
          <Button mode="default" onClick={logoutClick}>
            Login with PAT
          </Button>
        </div>
      );
    }

    if (envVars.consoleMode === 'cli' && persistedClient) {
      if (proMode === false) {
        return <ErrorText />;
      }

      return (
        <ApolloProvider client={persistedClient.client}>
          <div className={styles['metricsWrapper']}>
            <LeftPanel />
            <RightPanel>
              <Outlet />
            </RightPanel>
          </div>
        </ApolloProvider>
      );
    }
  };

  return renderMetrics();
};

const ErrorText = () => (
  <div className="p-6">
    <Text>
      Something went wrong! please refresh the page or reach out to us for more
      info
    </Text>
  </div>
);

export default Metrics;
