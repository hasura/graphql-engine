import { useEffect, useRef } from 'react';
import { Outlet, useLocation, useNavigate } from 'react-router';
import { FaChartLine, FaServer } from 'react-icons/fa';
import {
  EnterpriseNavbarButton,
  WithEELiteAccess,
  telemetryUserEventsTracker,
  Onboarding,
  CloudOnboarding,
  useCatalogState,
  useTelemetryStore,
  UpdateVersion,
  HeaderNavItem,
  MainHeader,
  checkAccess,
  hasDataAccess,
} from '@hasura/console-legacy-ce';
import { isMonitoringTabSupportedEnvironment } from '@hasura/shared/utils';
import './NotificationOverrides.css';
import { moduleName } from '../Services/Metrics/constants';
import { shouldRedirectToMetadataStatus } from './metadataStatusRedirect';
import { useAppContext } from '@hasura/shared/context';
import { CLI_CONSOLE_MODE, METADATA_STATUS_PATH } from '@hasura/shared/types';
import { useAuthContext } from '../../shared/auth/context';
import { useInconsistentMetadata, useMetadata } from '@hasura/metadata/api';
import { Badge, DropdownMenu, hasuraToast } from '@hasura/shared/ui';
import { InitializeTelemetry } from '@hasura/shared/analytics';
import { CiLogout } from 'react-icons/ci';

const Main = () => {
  const location = useLocation();
  const navigate = useNavigate();
  const { logout, privileges } = useAuthContext();
  const { refreshCatalogState } = useCatalogState();
  const { serverVersion, envVars } = useAppContext();
  const { consoleState } = useTelemetryStore();
  const accessState = checkAccess(privileges);
  const canAccessData = hasDataAccess(privileges);
  // the metadata API is admin-only. A 4xx response logs the user out, so
  // collaborators without data access (e.g. graphql_admin, view_metrics) skip it.
  const { data: meta } = useMetadata(undefined, {
    staleTime: 5 * 60000,
    enabled: canAccessData,
  });
  const { data: inconsistentMetadata, isLoading: inconsistentLoading } =
    useInconsistentMetadata(undefined, undefined, canAccessData);

  useEffect(() => {
    if (canAccessData) {
      refreshCatalogState();
    }

    if (envVars.isMetadataAPIEnabled === false) {
      const errorTitle = 'Metadata APIs are not accessible to the console';
      const errorMessage =
        'To use the console, please make sure that Metadata APIs are enabled';
      hasuraToast({
        type: 'error',
        title: errorTitle,
        message: errorMessage,
      });
    }

    document.querySelector('body')?.addEventListener('click', handleBodyClick);
  }, []);

  // Redirect to the metadata status page only once the inconsistent-metadata
  // query has RESOLVED and confirmed an actual inconsistency. The redirect used
  // to live in the mount-only effect above, where `inconsistentMetadata` is still
  // `undefined` (query pending) on first render — so `!undefined?.is_consistent`
  // was truthy and it redirected unconditionally, then never re-evaluated. Guard
  // on the loading/defined state and depend on it so it reacts to the resolved
  // (or re-fetched) result. Access policy (admin/admin-collaborator) is preserved.
  //
  // `navigate` changes identity on every location change (non-data router), so
  // read it from a ref: the redirect must only fire when the metadata
  // consistency changes, not on every navigation, or the console is locked on
  // the metadata status page.
  const navigateRef = useRef(navigate);
  useEffect(() => {
    navigateRef.current = navigate;
  });

  useEffect(() => {
    if (
      shouldRedirectToMetadataStatus({
        hasDataAccess: canAccessData,
        isLoading: inconsistentLoading,
        inconsistentMetadata,
      })
    ) {
      navigateRef.current(METADATA_STATUS_PATH);
    }
  }, [inconsistentLoading, inconsistentMetadata, canAccessData]);

  const handleBodyClick = (e) => {
    const heartDropDownOpen = document.querySelectorAll(
      '#dropdown_wrapper.open',
    );
    if (
      document.getElementById('dropdown_wrapper') &&
      !document.getElementById('dropdown_wrapper')?.contains(e.target) &&
      heartDropDownOpen.length !== 0
    ) {
      document.getElementById('dropdown_wrapper')?.classList.remove('open');
    }
  };

  const renderMetricsTab = () => {
    // still show the monitoring tab if the user login with admin secret
    if (
      !isMonitoringTabSupportedEnvironment(envVars) ||
      !accessState.hasMetricAccess
    ) {
      return null;
    }

    return (
      <HeaderNavItem
        title="Monitoring"
        icon={FaChartLine}
        tooltipText="Metrics"
        path={moduleName}
        pathname={location.pathname}
      />
    );
  };

  const renderProjectInfo = () => {
    const detailsPath = `${window.location.protocol}//${window.location.host}/project/${envVars.projectID}/details`;
    return envVars.consoleType === 'cloud' ? (
      <a
        href={detailsPath}
        target="_blank"
        rel="noopener noreferrer"
        title={envVars.projectName}
      >
        <Badge color="gray" variant="solid" highContrast>
          <FaServer aria-hidden="true" />
          <span>{envVars.projectName}</span>
        </Badge>
      </a>
    ) : null;
  };

  const renderTelemetrySetup = () => {
    return (
      <WithEELiteAccess>
        {({ access }) => {
          return (
            <InitializeTelemetry
              tracker={telemetryUserEventsTracker}
              skip={access === 'forbidden'}
            />
          );
        }}
      </WithEELiteAccess>
    );
  };

  return (
    <div>
      <div>
        <MainHeader
          serverVersion={serverVersion}
          isConsistentMetadata={inconsistentMetadata?.is_consistent ?? true}
          location={location}
          moreLeftItems={renderMetricsTab()}
          moreRightItems={
            <>
              <EnterpriseNavbarButton className="flex items-center normal-case font-normal text-slate-900 mr-2" />
              {renderTelemetrySetup()}
              {renderProjectInfo()}
            </>
          }
          accesses={accessState}
          moreUserMenuItems={
            // Cloud signs in via Hasura OAuth and the CLI supplies its own
            // credentials, so there's no console session to log out of.
            envVars.consoleType !== 'cloud' &&
            envVars.consoleMode !== CLI_CONSOLE_MODE
              ? [
                  <DropdownMenu.Item onSelect={logout}>
                    <CiLogout />
                    Logout
                  </DropdownMenu.Item>,
                ]
              : []
          }
        />
      </div>

      <div>
        <Outlet />
        {serverVersion &&
        accessState &&
        'hasDataAccess' in accessState &&
        accessState.hasDataAccess ? (
          <Onboarding console_opts={consoleState} metadata={meta?.metadata} />
        ) : null}
      </div>
      <CloudOnboarding />
      {consoleState && envVars.consoleType !== 'cloud' ? (
        <UpdateVersion consoleState={consoleState} />
      ) : null}
    </div>
  );
};

export default Main;
