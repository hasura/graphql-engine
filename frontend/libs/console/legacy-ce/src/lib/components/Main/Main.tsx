import { useEffect } from 'react';
import { Outlet, useLocation, useNavigate } from 'react-router';
import Onboarding from '../Common/Onboarding';
import { UpdateVersion } from './components/UpdateVersion';
import styles from './Main.module.scss';
import { useTelemetryStore } from '../../telemetry/store';
import useCatalogState from '../../telemetry/useCatalogState';
import { useAppContext } from '@hasura/shared/context';
import ErrorBoundary from '../Error/ErrorBoundary';
import { METADATA_STATUS_PATH } from '@hasura/shared/types';
import { useMetadata, useReloadMetadata } from '@hasura/metadata/api';
import MainHeader from './components/MainHeader';
import { defaultAccessState } from '../../shared/auth';

const Main = () => {
  const navigate = useNavigate();
  const location = useLocation();
  const { serverVersion } = useAppContext();
  const { consoleState } = useTelemetryStore();
  const { refreshCatalogState } = useCatalogState();
  const {
    reloadMetadata,
    inconsistentMetadata,
    isLoading: inconsistentLoading,
  } = useReloadMetadata();
  const { data: meta } = useMetadata();

  useEffect(() => {
    refreshCatalogState();
  }, []);

  useEffect(() => {
    if (inconsistentLoading || !inconsistentMetadata) {
      return;
    }

    if (!inconsistentMetadata.is_consistent) {
      navigate(METADATA_STATUS_PATH);
      return;
    }
  }, [inconsistentLoading, inconsistentMetadata]);

  return (
    <ErrorBoundary
      location={location}
      navigate={navigate}
      reloadMetadata={reloadMetadata}
    >
      <div className={styles.container}>
        <Onboarding console_opts={consoleState} metadata={meta?.metadata} />
        <div>
          <MainHeader
            isConsistentMetadata={inconsistentMetadata?.is_consistent ?? true}
            location={location}
            serverVersion={serverVersion}
            accesses={defaultAccessState}
          />
          <div className={styles.main + ' container-fluid'}>
            <Outlet />
          </div>
          {consoleState ? <UpdateVersion consoleState={consoleState!} /> : null}
        </div>
      </div>
    </ErrorBoundary>
  );
};

export default Main;
