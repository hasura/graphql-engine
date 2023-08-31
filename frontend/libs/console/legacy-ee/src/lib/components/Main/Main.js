import React from 'react';
import { connect } from 'react-redux';
import { Link } from 'react-router';
import {
  FaCog,
  FaCogs,
  FaExclamationCircle,
  FaExclamationTriangle,
  FaFlask,
  FaDatabase,
  FaUser,
  FaCloud,
  FaPlug,
  FaChartLine,
  FaQuestionCircle,
  FaServer,
} from 'react-icons/fa';
import clsx from 'clsx';

import HeaderNavItem, {
  linkStyle,
  activeLinkStyle,
  itemContainerStyle,
} from './HeaderNavItem';

import * as DropdownMenu from '@radix-ui/react-dropdown-menu';

import globals from '../../Globals';
import 'react-toggle/style.css';
import {
  Spinner,
  EntepriseNavbarButton,
  WithEELiteAccess,
  InitializeTelemetry,
  telemetryUserEventsTracker,
  Analytics,
  isEEClassicConsole,
} from '@hasura/console-legacy-ce';

import {
  Badge,
  NotificationSection,
  Onboarding,
  CloudOnboarding,
  Tooltip,
  fetchConsoleNotifications,
  loadInconsistentObjects,
  redirectToMetadataStatus,
  updateRequestHeaders,
  showErrorNotification,
  isMonitoringTabSupportedEnvironment,
  isCloudConsole,
  ControlPlane,
} from '@hasura/console-legacy-ce';

import { versionGT, FT_JWT_ANALYZER } from '../../helpers/versionUtils';
import {
  loadServerVersion,
  fetchServerConfig,
  loadLatestServerVersion,
  featureCompatibilityInit,
  clearCollaboratorSignInState,
  loadLuxProjectInfo,
  loadLuxProjectEntitlements,
} from './Actions';
import './NotificationOverrides.css';
import {
  LS_KEYS,
  removeLSItem,
  getLSItem,
  setLSItem,
} from '@hasura/console-legacy-ce';

import { moduleName, relativeModulePath } from '../Services/Metrics/constants';

import {
  getLoveConsentState,
  setLoveConsentState,
} from './loveConsentLocalStorage';

import {
  decodeToken,
  checkAccess,
  defaultAccessState,
} from '../../utils/computeAccess';
import { restrictedPathsMetadata } from '../../utils/redirectUtils';
import extendedGlobals from '../../Globals';

import { appcuesIdentify } from '../../utils/appCues';
import styles from './Main.module.scss';
import logo from './images/white-logo.svg';
import logoutIcon from './images/log-out.svg';
import EELogo from './images/hasura-ee-mono-light.svg';
import { isHasuraCollaboratorUser } from '../Login/utils';
import {
  ConsoleDevTools,
  isFeatureFlagEnabled,
  availableFeatureFlagIds,
} from '@hasura/console-legacy-ce';

const { Plan, Project_Entitlement_Types_Enum } = ControlPlane;
class Main extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      showUpdateNotification: false,
      loveConsentState: getLoveConsentState(),
      isDropdownOpen: false,
    };

    this.handleBodyClick = this.handleBodyClick.bind(this);
    this.hasMetadataEnabledConfig = Object.keys(extendedGlobals).includes(
      'isMetadataAPIEnabled'
    );
  }

  componentDidMount() {
    const { dispatch } = this.props;

    appcuesIdentify();
    dispatch(loadLuxProjectInfo());

    dispatch(loadLuxProjectEntitlements());

    if (
      this.hasMetadataEnabledConfig &&
      extendedGlobals.isMetadataAPIEnabled === false
    ) {
      const errorTitle = 'Metadata APIs are not accessible to the console';
      const errorMessage =
        'To use the console, please make sure that Metadata APIs are enabled';
      dispatch(showErrorNotification(errorTitle, errorMessage));
    }

    // clear graphiql headers from localstorage on team console unload for non-admin user
    if (window.__env.userRole === 'user' && !!globals.tenantID) {
      window.addEventListener('unload', () => {
        removeLSItem(LS_KEYS.apiExplorerConsoleGraphQLHeaders);
      });
    }

    document
      .querySelector('body')
      .addEventListener('click', this.handleBodyClick);

    // Function to update notification
    updateRequestHeaders(this.props);

    dispatch(loadServerVersion()).then(() => {
      dispatch(featureCompatibilityInit());

      // metadata related should only be fetched if the user is an `admin` or a `admin` collaborator
      const { accessState } = this.props;
      if (
        accessState &&
        'hasDataAccess' in accessState &&
        accessState.hasDataAccess
      ) {
        dispatch(loadInconsistentObjects({ shouldReloadMetadata: false })).then(
          () => {
            this.handleMetadataRedirect();
          }
        );
      }
      // dispatch(loadConsoleTelemetryOpts());
      dispatch(loadLatestServerVersion()).then(() => {
        this.setShowUpdateNotification();
      });
      // Should only concern with notifications if the user has `admin` access
      if (
        accessState &&
        'hasDataAccess' in accessState &&
        accessState.hasDataAccess
      ) {
        dispatch(fetchConsoleNotifications());
      }
    });
  }

  componentDidUpdate(prevProps) {
    const prevHeaders = Object.keys(prevProps.requestHeaders);
    const currHeaders = Object.keys(this.props.requestHeaders);

    if (
      prevHeaders.length !== currHeaders.length ||
      prevHeaders.filter(hdr => !currHeaders.includes(hdr)).length
    ) {
      updateRequestHeaders(this.props);
    }
  }

  setShowUpdateNotification() {
    const { latestServerVersion, serverVersion } = this.props;

    try {
      const isClosedBefore = getLSItem(
        latestServerVersion + '_BANNER_NOTIFICATION_CLOSED'
      );

      if (isClosedBefore !== 'true') {
        const isUpdateAvailable = versionGT(latestServerVersion, serverVersion);

        if (isUpdateAvailable) {
          this.setState({
            showUpdateNotification: true,
          });
        }
      }
    } catch (e) {
      console.error(e);
    }
  }
  componentWillReceiveProps(nextProps) {
    const { [FT_JWT_ANALYZER]: currJwtAnalyzerCompatibility } =
      this.props.featuresCompatibility;
    const { [FT_JWT_ANALYZER]: nextJwtAnalyzerCompatibility } =
      nextProps.featuresCompatibility;

    const { accessState } = nextProps;

    if ('hasGraphQLAccess' in accessState && accessState.hasGraphQLAccess) {
      if (
        currJwtAnalyzerCompatibility !== nextJwtAnalyzerCompatibility &&
        nextJwtAnalyzerCompatibility
      ) {
        this.fetchServerConfig();
      }
    }
  }

  fetchServerConfig() {
    const { dispatch } = this.props;

    dispatch(fetchServerConfig());
  }

  handleBodyClick(e) {
    const heartDropDownOpen = document.querySelectorAll(
      '#dropdown_wrapper.open'
    );
    if (
      document.getElementById('dropdown_wrapper') &&
      !document.getElementById('dropdown_wrapper').contains(e.target) &&
      heartDropDownOpen.length !== 0
    ) {
      document.getElementById('dropdown_wrapper').classList.remove('open');
    }
  }

  handleDropdownToggle() {
    document.getElementById('dropdown_wrapper').classList.toggle('open');
  }
  handleMetadataRedirect() {
    if (this.props.metadata.inconsistentObjects.length > 0) {
      this.props.dispatch(redirectToMetadataStatus());
    }
  }

  closeLoveIcon() {
    const s = {
      isDismissed: true,
    };
    setLoveConsentState(s);
    this.setState({
      loveConsentState: { ...getLoveConsentState() },
    });
  }

  closeUpdateBanner() {
    const { latestServerVersion } = this.props;
    setLSItem(latestServerVersion + '_BANNER_NOTIFICATION_CLOSED', 'true');
    this.setState({ showUpdateNotification: false });
  }
  closeDropDown = () => {
    this.setState({
      isDropdownOpen: false,
    });
  };

  toggleDropDown = () => {
    this.setState(prevState => ({
      isDropdownOpen: !prevState.isDropdownOpen,
    }));
  };

  invalidateToken() {
    /*
    const { dispatch } = this.props;
    dispatch(push('/'));
    */
    /* TODO:
     * Total hack
     * */
    window.location.href = '/';
  }

  logoutCollaboratorWorkFlow() {
    const { dispatch } = this.props;
    dispatch(clearCollaboratorSignInState());
  }

  /**
   * checks if current project is an enterprise project
   * NOTE: an enterprise project is any non free plan project
   * created by a Hasura enterprise user
   */
  isEnterpriseProject() {
    return (
      this.props?.project?.is_enterprise_user &&
      this.props?.project?.plan_name !== 'cloud_free'
    );
  }

  /**
   * if a project is on the new cloud_free_v2 plan
   * metrics are probably not enabled
   * check if entitlement is enabled for such plans
   */
  hasMetricsEntitlement() {
    // if not a cloud console, don't check
    if (!isCloudConsole(globals)) {
      return true;
    }

    // get the plan name and entitlements array from the project
    const {
      project: { plan_name = '' },
      projectEntitlements = [],
    } = this.props;

    // entitlements are added only for projects on the
    //  new cloud_free_v2 and cloud_shared plans
    // so if the plan is not one of these, return true
    if (
      plan_name === Plan.CloudFree ||
      plan_name === Plan.CloudPayg ||
      plan_name === 'pro' ||
      plan_name === 'cloud_dedicated_vpc'
    ) {
      return true;
    }

    // if the plan is one of the new plans, check if the
    // metrics entitlement is enabled
    const { entitlement: { config_is_enabled } = {} } =
      projectEntitlements.find(
        ({ entitlement: { type } }) =>
          type === Project_Entitlement_Types_Enum.ConsoleMetricsTab
      ) || {};

    // if the entitlement is enabled, return true
    return !!config_is_enabled;
  }

  render() {
    const {
      children,
      location,
      migrationModeProgress,
      currentSchema,
      currentSource,
      serverVersion,
      metadata,
      accessState,
    } = this.props;

    const appPrefix = '';

    const currentLocation = location.pathname;
    const currentActiveBlock = currentLocation.split('/')[1];

    const getMainContent = () => {
      let mainContent = null;

      if (!migrationModeProgress) {
        mainContent = children && React.cloneElement(children);
      } else {
        mainContent = (
          <div>
            {' '}
            <Spinner />{' '}
          </div>
        );
      }
      return mainContent;
    };

    const getMetadataIcon = () => {
      if (metadata.inconsistentObjects.length === 0) {
        return <FaCog />;
      }

      return (
        <div className="relative">
          <FaCog />
          <div className="absolute -top-2 left-2 ">
            <FaExclamationCircle
              className="bg-white rounded-full"
              color="#d9534f"
            />
          </div>
        </div>
      );
    };

    const getAdminSecretSection = () => {
      let adminSecretHtml = null;

      if (!globals.isAdminSecretSet) {
        adminSecretHtml = (
          <Tooltip
            side="bottom"
            tooltipContentChildren={`This graphql endpoint is public and you should add an ${globals.adminSecretLabel}`}
          >
            <div className={itemContainerStyle}>
              <a
                className={clsx(linkStyle)}
                href="https://hasura.io/docs/latest/deployment/securing-graphql-endpoint/"
                target="_blank"
                rel="noopener noreferrer"
              >
                <Badge color="yellow">
                  <FaExclamationTriangle />
                  &nbsp;Secure your endpoint
                </Badge>
              </a>
            </div>
          </Tooltip>
        );
      }
      return adminSecretHtml;
    };

    const getConsolidatedPath = (path, pathPrefix) => {
      if (restrictedPathsMetadata[pathPrefix]) {
        if (
          !accessState[restrictedPathsMetadata[pathPrefix].keyInAccessState]
        ) {
          return restrictedPathsMetadata[pathPrefix].replace;
        }
      }
      return path;
    };

    const getDataPath = () => {
      const perfMode = isFeatureFlagEnabled(
        availableFeatureFlagIds.performanceMode
      );

      if (perfMode) return 'data/';

      if (currentSource && currentSchema) {
        return `data/${currentSource}/schema/${currentSchema}`;
      }
      return 'data/';
    };

    const renderMetricsTab = () => {
      if (
        'hasMetricAccess' in accessState &&
        accessState.hasMetricAccess &&
        isMonitoringTabSupportedEnvironment(globals) &&
        (isCloudConsole(globals) || isHasuraCollaboratorUser()) &&
        this.hasMetricsEntitlement()
      ) {
        return (
          <HeaderNavItem
            title="Monitoring"
            icon={<FaChartLine aria-hidden="true" />}
            tooltipText="Metrics"
            itemPath={moduleName}
            linkPath={relativeModulePath}
            appPrefix={appPrefix}
            currentActiveBlock={currentActiveBlock}
          />
        );
      }
      return null;
    };

    const renderLogout = () => {
      // userRole is only being set for team console
      return extendedGlobals.consoleType !== 'cloud' ? (
        <div className={itemContainerStyle}>
          <DropdownMenu.Root>
            {/* Compensate legacy styles directly with style attribute */}
            <DropdownMenu.Trigger
              noCaret
              className={clsx(
                linkStyle,
                'data-state-open:bg-slate-900 data-state-closed:!text-white data-state-open:!text-primary'
              )}
              style={{
                border: 'none',
              }}
            >
              <span className="text-sm self-baseline">
                <FaUser aria-hidden="true" />
              </span>
            </DropdownMenu.Trigger>
            <DropdownMenu.Portal>
              <DropdownMenu.Content
                className="max-w-md -translate-x-6 translate-y-2 data-state-open:animate-dropdownMenuContentOpen data-state-closed:animate-dropdownMenuContentClose"
                sideOffset={8}
              >
                <DropdownMenu.Item
                  className="shadow-lg bg-white hover:bg-slate-100 p-4 rounded overflow-hidden cursor-pointer"
                  onSelect={this.logoutCollaboratorWorkFlow.bind(this)}
                >
                  <img
                    className={styles.navBarIcon}
                    src={logoutIcon}
                    alt={'logout'}
                  />{' '}
                  Logout
                </DropdownMenu.Item>
              </DropdownMenu.Content>
            </DropdownMenu.Portal>
          </DropdownMenu.Root>
        </div>
      ) : null;
    };

    const renderProjectInfo = () => {
      const detailsPath = `${window.location.protocol}//${window.location.host}/project/${globals.hasuraCloudProjectId}/details`;
      return window.__env.consoleType === 'cloud' ? (
        <div className={clsx(itemContainerStyle, 'ml-0')}>
          <a
            className={linkStyle}
            href={detailsPath}
            target="_blank"
            rel="noopener noreferrer"
          >
            <Badge>
              <FaServer />
              &nbsp;
              <span>{globals.projectName}</span>
            </Badge>
          </a>
        </div>
      ) : null;
    };

    const renderMetadataIcon = () => (
      <div className={itemContainerStyle}>
        <Analytics name="navbar-settings">
          <Link
            className={clsx(
              linkStyle,
              currentActiveBlock === 'settings' && activeLinkStyle
            )}
            to={getConsolidatedPath('/settings', '/settings', accessState)}
          >
            <span className="text-sm self-baseline">{getMetadataIcon()}</span>
            <span className="uppercase text-left">Settings</span>
          </Link>
        </Analytics>
      </div>
    );

    const getLogo = () => {
      return (
        <WithEELiteAccess globals={globals}>
          {({ access }) => {
            const getLogoSrc = () => {
              if (this.isEnterpriseProject() || isEEClassicConsole()) {
                return EELogo;
              }
              if (access === 'active') {
                return EELogo;
              }
              return logo;
            };
            return <img className="w-24" src={getLogoSrc()} alt="HasuraLogo" />;
          }}
        </WithEELiteAccess>
      );
    };

    const renderTelemetrySetup = () => {
      return (
        <WithEELiteAccess globals={globals}>
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
      <div className={styles.container}>
        <div className={styles.flexRow}>
          <div className="font-sans bg-slate-700 text-slate-100 flex h-16">
            <div className="flex gap-1 flex-grow">
              <div className="px-5 py-2 flex items-center gap-3">
                <Link
                  to={accessState.hasGraphQLAccess ? '/' : '/access-denied'}
                >
                  {getLogo()}
                </Link>
                <Link
                  to={accessState.hasGraphQLAccess ? '/' : '/access-denied'}
                >
                  <div className="text-white text-xs max-w-[128px]">
                    {serverVersion}
                  </div>
                </Link>
              </div>
              <ul className="flex gap-2" data-testid="Nav bar">
                <HeaderNavItem
                  title="API"
                  icon={<FaFlask aria-hidden="true" />}
                  tooltipText="Test the GraphQL APIs"
                  itemPath="api"
                  linkPath={getConsolidatedPath(
                    '/api/api-explorer',
                    '/api',
                    accessState
                  )}
                  appPrefix={appPrefix}
                  currentActiveBlock={currentActiveBlock}
                  isDefault
                />
                <HeaderNavItem
                  title="Data"
                  icon={<FaDatabase aria-hidden="true" />}
                  tooltipText="Data & Schema management"
                  itemPath="data"
                  linkPath={getConsolidatedPath(
                    getDataPath(),
                    '/data',
                    accessState
                  )}
                  appPrefix={appPrefix}
                  currentActiveBlock={currentActiveBlock}
                />
                <HeaderNavItem
                  title="Actions"
                  icon={<FaCogs aria-hidden="true" />}
                  tooltipText="Manage Actions"
                  itemPath="actions"
                  linkPath={getConsolidatedPath(
                    '/actions/manage/actions',
                    '/actions',
                    accessState
                  )}
                  appPrefix={appPrefix}
                  currentActiveBlock={currentActiveBlock}
                />
                <HeaderNavItem
                  title="Remote Schemas"
                  icon={<FaPlug aria-hidden="true" />}
                  tooltipText="Manage Remote Schemas"
                  itemPath="remote-schemas"
                  linkPath={getConsolidatedPath(
                    '/remote-schemas/manage/schemas',
                    '/remote-schemas',
                    accessState
                  )}
                  appPrefix={appPrefix}
                  currentActiveBlock={currentActiveBlock}
                />
                <HeaderNavItem
                  title="Events"
                  icon={<FaCloud aria-hidden="true" />}
                  tooltipText="Manage Event and Scheduled Triggers"
                  itemPath="events"
                  linkPath={getConsolidatedPath(
                    '/events/data/manage',
                    '/events',
                    accessState
                  )}
                  appPrefix={appPrefix}
                  currentActiveBlock={currentActiveBlock}
                />
                {renderMetricsTab()}
              </ul>
            </div>
            <div className="bootstrap-jail">
              <div
                id="dropdown_wrapper"
                className={clsx(
                  'flex gap-2 justify-end items-stretch relative mr-4 h-full',
                  this.state.isDropdownOpen ? 'open' : ''
                )}
              >
                {getAdminSecretSection()}
                <EntepriseNavbarButton className="flex items-center normal-case font-normal text-slate-900 mr-sm" />
                {renderTelemetrySetup()}
                {renderProjectInfo()}
                {renderMetadataIcon()}
                <div className={itemContainerStyle}>
                  <a
                    id="help"
                    className={clsx(linkStyle)}
                    href={'https://hasura.io/help'}
                    target="_blank"
                    rel="noopener noreferrer"
                  >
                    <span className="text-sm self-baseline">
                      <FaQuestionCircle />
                    </span>
                    <span className="uppercase text-left">HELP</span>
                  </a>
                </div>
                {serverVersion &&
                accessState &&
                'hasDataAccess' in accessState &&
                accessState.hasDataAccess ? (
                  <NotificationSection
                    isDropDownOpen={this.state.isDropdownOpen}
                    closeDropDown={this.closeDropDown}
                    toggleDropDown={this.toggleDropDown}
                  />
                ) : null}
                {renderLogout()}
              </div>
            </div>
          </div>

          <div className={styles.main + ' container-fluid'}>
            {getMainContent()}
            {serverVersion &&
            accessState &&
            'hasDataAccess' in accessState &&
            accessState.hasDataAccess ? (
              <Onboarding
                dispatch={this.props.dispatch}
                console_opts={this.props.console_opts}
                metadata={metadata?.metadataObject}
              />
            ) : null}
          </div>
          <CloudOnboarding />
        </div>
        <ConsoleDevTools />
      </div>
    );
  }
}

export const decodeJWT = oAuthResponse => {
  if ('id_token' in oAuthResponse) {
    const decoded = decodeToken(oAuthResponse.id_token);
    if (!decoded) {
      // this.invalidateToken();
      window.location.href = '/';
    }
    // console.log(decoded);
    const { payload } = decoded;
    const { collaborator_privileges } = payload;

    const accessObj = checkAccess(collaborator_privileges);
    // console.log(accessObj);
    const accessState = { ...accessObj };
    return accessState;
  }
  return { ...defaultAccessState };
};

const mapStateToProps = (state, ownProps) => {
  const { project } = state.main;

  return {
    ...state.main,
    header: state.header,
    pathname: ownProps.location.pathname,
    currentSchema: state.tables.currentSchema,
    currentSource: state.tables.currentDataSource,
    metadata: state.metadata,
    console_opts: state.telemetry.console_opts,
    accessState: decodeJWT(state.main.oAuthResponse),
    projectName: project.name,
    projectId: project.id,
    requestHeaders: state.tables.dataHeaders,
    projectEntitlements: state.main.projectEntitlements,
  };
};

export default connect(mapStateToProps)(Main);
