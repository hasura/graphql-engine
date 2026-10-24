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

const Metrics = () => {
  const { envVars } = useAppContext();
  const { data: projectInfo, isFetching: projectInfoLoading } =
    useProjectInfo();
  const { logout, authType, getMetricsHeaders } = useAuthContext();
  const [connection, setConnection] = React.useState<{
    client: ApolloClientOutput;
    projectInfo: typeof projectInfo;
    getHeaders: typeof getMetricsHeaders;
    authType: typeof authType;
  } | null>(null);
  const [clientError, setClientError] = React.useState(false);
  // Never render a client made for an earlier project or session.
  const persistedClient =
    connection?.projectInfo === projectInfo &&
    connection?.getHeaders === getMetricsHeaders &&
    connection?.authType === authType
      ? connection.client
      : null;

  React.useEffect(() => {
    setClientError(false);
    if (!projectInfo || (authType !== 'hasura-sso' && authType !== 'pat')) {
      return;
    }

    let cancelled = false;
    let client: ApolloClientOutput | undefined;
    getMetricsHeaders()
      .then((metricsHeaders) => {
        if (cancelled) return;
        client = makeApolloClient(
          getMetricsUrl(projectInfo.metricsFQDN),
          metricsHeaders,
        );
        setConnection({
          client,
          projectInfo,
          getHeaders: getMetricsHeaders,
          authType,
        });
      })
      .catch(() => {
        if (!cancelled) setClientError(true);
      });

    return () => {
      cancelled = true;
      if (client) {
        void disposeApolloClient(client).catch(() => {
          // Teardown-only: the client may already have been disposed.
        });
      }
    };
  }, [projectInfo, authType, getMetricsHeaders]);

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

      if (!projectInfo?.id) {
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

      const projectId = projectInfo.id;

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

      if (!clientError && (authType === 'hasura-sso' || authType === 'pat')) {
        return (
          <div className="p-6">
            <Skeleton width="100%" height="20px" />
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
