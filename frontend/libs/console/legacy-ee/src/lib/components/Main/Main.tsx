import { useEffect } from 'react';
import { Outlet, useLocation, useNavigate } from 'react-router';
import { FaChartLine, FaServer } from 'react-icons/fa';
import clsx from 'clsx';
import {
  EnterpriseNavbarButton,
  WithEELiteAccess,
  telemetryUserEventsTracker,
  Onboarding,
  CloudOnboarding,
  ControlPlane,
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
import { useProjectInfo } from '../../hooks/useProjectInfo';
import { METADATA_STATUS_PATH } from '@hasura/shared/types';
import useEnterpriseAuth from '../../shared/auth/useEnterpriseAuth';
import { useInconsistentMetadata, useMetadata } from '@hasura/metadata/api';
import { Badge, DropdownMenu, hasuraToast } from '@hasura/shared/ui';
import { InitializeTelemetry } from '@hasura/shared/analytics';
import { CiLogout } from 'react-icons/ci';

const { Plan, Project_Entitlement_Types_Enum } = ControlPlane;

const Main = () => {
  const location = useLocation();
  const navigate = useNavigate();
  const { logout, privileges } = useEnterpriseAuth();
  const { refreshCatalogState } = useCatalogState();
  const { serverVersion, envVars } = useAppContext();
  const { consoleState } = useTelemetryStore();
  const { data: meta } = useMetadata();
  const { data: inconsistentMetadata, isLoading: inconsistentLoading } =
    useInconsistentMetadata();
  const { data: projectInfo } = useProjectInfo();
  const accessState = checkAccess(privileges);

  useEffect(() => {
    refreshCatalogState();

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
  useEffect(() => {
    if (
      shouldRedirectToMetadataStatus({
        hasDataAccess: hasDataAccess(privileges),
        isLoading: inconsistentLoading,
        inconsistentMetadata,
      })
    ) {
      navigate(METADATA_STATUS_PATH);
    }
  }, [inconsistentLoading, inconsistentMetadata, privileges, navigate]);

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

  /**
   * if a project is on the new cloud_free_v2 plan
   * metrics are probably not enabled
   * check if entitlement is enabled for such plans
   */
  const hasMetricsEntitlement = () => {
    // get the plan name and entitlements array from the project
    const plan_name = projectInfo?.plan_name ?? '';

    // entitlements are added only for projects on the
    //  new cloud_free_v2 and cloud_shared plans
    // so if the plan is not one of these, return true
    if (
      plan_name === Plan.CloudFree ||
      plan_name === Plan.CloudPayg ||
      plan_name === Plan.Pro ||
      plan_name === Plan.CloudDedicatedVPC
    ) {
      return true;
    }

    // if the plan is one of the new plans, check if the
    // metrics entitlement is enabled
    const { entitlement: { config_is_enabled } = {} } =
      (projectInfo?.entitlements ?? []).find(
        ({ entitlement: { type } }) =>
          type === Project_Entitlement_Types_Enum.ConsoleMetricsTab,
      ) || {};

    // if the entitlement is enabled, return true
    return !!config_is_enabled;
  };

  const renderMetricsTab = () => {
    if (!isMonitoringTabSupportedEnvironment(envVars)) {
      return null;
    }

    // still show the monitoring tab if the user login with admin secret
    if (
      !accessState.hasMetricAccess ||
      (envVars.consoleType !== 'pro' && !hasMetricsEntitlement())
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
      <div className={clsx('ml-0')}>
        <a href={detailsPath} target="_blank" rel="noopener noreferrer">
          <Badge>
            <FaServer />
            &nbsp;
            <span>{envVars.projectName}</span>
          </Badge>
        </a>
      </div>
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
            envVars.consoleType !== 'cloud'
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
        <UpdateVersion consoleState={consoleState!} />
      ) : null}
    </div>
  );
};

export default Main;
