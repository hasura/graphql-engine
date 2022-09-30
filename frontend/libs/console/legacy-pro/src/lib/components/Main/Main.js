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
} from 'react-icons/fa';

import Dropdown from 'react-bootstrap/lib/Dropdown';
import MenuItem from 'react-bootstrap/lib/MenuItem';

import globals from '../../Globals';
import 'react-toggle/style.css';
import { Spinner } from '@hasura/console-oss';
import {
  NotificationSection,
  Onboarding,
  OnboardingWizard,
  Tooltip,
  fetchConsoleNotifications,
  loadInconsistentObjects,
  redirectToMetadataStatus,
  updateRequestHeaders,
  showErrorNotification,
  isMonitoringTabSupportedEnvironment,
} from '@hasura/console-oss';
import { versionGT, FT_JWT_ANALYZER } from '../../helpers/versionUtils';
import {
  loadServerVersion,
  fetchServerConfig,
  loadLatestServerVersion,
  featureCompatibilityInit,
  clearCollaboratorSignInState,
  loadLuxProjectInfo,
} from './Actions';
import './NotificationOverrides.css';
import { clearPersistedGraphiQLHeaders } from '../../utils/localstorage';
import { growthExperimentsClient } from '../../utils/growthExperimentsClient';

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

const UpperCase = ({ children }) => <p className="uppercase">{children}</p>;

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
        clearPersistedGraphiQLHeaders();
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
      const isClosedBefore = window.localStorage.getItem(
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
    const {
      [FT_JWT_ANALYZER]: currJwtAnalyzerCompatibility,
    } = this.props.featuresCompatibility;
    const {
      [FT_JWT_ANALYZER]: nextJwtAnalyzerCompatibility,
    } = nextProps.featuresCompatibility;

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
    window.localStorage.setItem(
      latestServerVersion + '_BANNER_NOTIFICATION_CLOSED',
      'true'
    );
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

    const styles = require('./Main.scss');

    const appPrefix = '';

    const logo = require('./images/white-logo.svg');

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

    const getMetadataSelectedMarker = () => {
      let metadataSelectedMarker = null;

      if (currentActiveBlock === 'settings') {
        metadataSelectedMarker = <span className={styles.selected} />;
      }

      return metadataSelectedMarker;
    };

    const getMetadataIcon = () => {
      if (metadata.inconsistentObjects.length === 0) {
        return <FaCog className={`${styles.question} text-lg`} />;
      }
      return (
        <div className={styles.question}>
          <FaCog className="text-lg" />
          <div className={styles.overlappingExclamation}>
            <div className={styles.iconWhiteBackground} />
            <div>
              <FaExclamationCircle />
            </div>
          </div>
        </div>
      );
    };

    const getAdminSecretSection = () => {
      let adminSecretHtml = null;

      if (!globals.isAdminSecretSet) {
        adminSecretHtml = (
          <div className={styles.secureSection}>
            <Tooltip
              side="left"
              tooltipContentChildren={`This graphql endpoint is public and you should add an ${globals.adminSecretLabel}`}
            >
              <a
                href="https://docs.hasura.io/1.0/graphql/manual/deployment/securing-graphql-endpoint.html"
                target="_blank"
                rel="noopener noreferrer"
              >
                <FaExclamationTriangle className={styles.padd_small_right} />
              </a>
            </Tooltip>
          </div>
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

    const renderGraphQLTab = () => {
      return (
        <Tooltip side="right" tooltipContentChildren="Test the GraphQL APIs">
          <li>
            <Link
              className={
                currentActiveBlock === 'api' || currentActiveBlock === ''
                  ? styles.navSideBarActive
                  : ''
              }
              to={
                appPrefix +
                getConsolidatedPath('/api/api-explorer', '/api', accessState)
              }
            >
              <div className={styles.iconCenter} data-test="api-explorer">
                <FaFlask title="API Explorer" aria-hidden="true" />
              </div>
              <UpperCase>API</UpperCase>
            </Link>
          </li>
        </Tooltip>
      );
    };

    const getDataPath = () => {
      if (currentSource && currentSchema) {
        return `data/${currentSource}/schema/${currentSchema}`;
      }
      return 'data/';
    };

    const renderDataTab = () => {
      return (
        <Tooltip side="right" tooltipContentChildren="Data & Schema management">
          <li>
            <Link
              className={
                currentActiveBlock === 'data' ? styles.navSideBarActive : ''
              }
              to={
                appPrefix +
                getConsolidatedPath(getDataPath(), '/data', accessState)
              }
            >
              <div className={styles.iconCenter}>
                <FaDatabase title="Data Service" aria-hidden="true" />
              </div>
              <UpperCase>Data</UpperCase>
            </Link>
          </li>
        </Tooltip>
      );
    };
    const renderActionsTab = () => {
      return (
        <Tooltip side="right" tooltipContentChildren="Manage Actions">
          <li>
            <Link
              className={
                currentActiveBlock === 'actions' ? styles.navSideBarActive : ''
              }
              to={
                appPrefix +
                getConsolidatedPath(
                  '/actions/manage/actions',
                  '/actions',
                  accessState
                )
              }
            >
              <div className={styles.iconCenter}>
                <FaCogs title="Actions" aria-hidden="true" />
              </div>
              <UpperCase>Actions</UpperCase>
            </Link>
          </li>
        </Tooltip>
      );
    };
    const renderLogout = () => {
      const logoutIcon = require('./images/log-out.svg');
      // userRole is only being set for team console
      return extendedGlobals.consoleType !== 'cloud' ? (
        <Dropdown
          id="dropdown-custom-menu"
          className={`${styles.dropDownContainer} ${styles.flexCenterContainer}`}
        >
          <Dropdown.Toggle noCaret className={styles.userBtn}>
            <FaUser aria-hidden="true" className="text-white" />
          </Dropdown.Toggle>
          <Dropdown.Menu className={styles.dropdownUl}>
            <MenuItem onClick={this.logoutCollaboratorWorkFlow.bind(this)}>
              <img
                className={styles.navBarIcon}
                src={logoutIcon}
                alt={'logout'}
              />{' '}
              Logout
            </MenuItem>
          </Dropdown.Menu>
        </Dropdown>
      ) : null;
    };
    const renderProjectInfo = () => {
      const projectImg = require('./images/project.svg');
      const detailsPath = `${window.location.protocol}//${window.location.host}/project/${globals.hasuraCloudProjectId}/details`;
      return window.__env.consoleType === 'cloud' ? (
        <span className={styles.projectInfo}>
          <img src={projectImg} />
          <a href={detailsPath} target="_blank" rel="noopener noreferrer">
            {globals.projectName}
          </a>
        </span>
      ) : null;
    };

    const renderMetadataIcon = () => (
      <Link to={getConsolidatedPath('/settings', '/settings', accessState)}>
        <div className={styles.headerRightNavbarBtn}>
          {getMetadataIcon()}
          {getMetadataSelectedMarker()}
        </div>
      </Link>
    );

    const renderEventsTab = () => {
      return (
        <Tooltip side="right" tooltipContentChildren="Manage Event Triggers">
          <li>
            <Link
              className={
                currentActiveBlock === 'events' ? styles.navSideBarActive : ''
              }
              to={
                appPrefix +
                getConsolidatedPath(
                  '/events/data/manage',
                  '/events',
                  accessState
                )
              }
            >
              <div className={styles.iconCenter}>
                <FaCloud title="Events" aria-hidden="true" />
              </div>
              <UpperCase>Events</UpperCase>
            </Link>
          </li>
        </Tooltip>
      );
    };

    const renderRemoteTab = () => {
      return (
        <Tooltip side="right" tooltipContentChildren="Manage Remote Schemas">
          <li>
            <Link
              className={
                currentActiveBlock === 'remote-schemas'
                  ? styles.navSideBarActive
                  : ''
              }
              to={
                appPrefix +
                getConsolidatedPath(
                  '/remote-schemas/manage/schemas',
                  '/remote-schemas',
                  accessState
                )
              }
            >
              <div className={styles.iconCenter}>
                <FaPlug title="Remote Schemas" aria-hidden="true" />
              </div>
              <UpperCase>Remote Schemas</UpperCase>
            </Link>
          </li>
        </Tooltip>
      );
    };

    const renderMetricsTab = () => {
      if (
        'hasMetricAccess' in accessState &&
        accessState.hasMetricAccess &&
        isMonitoringTabSupportedEnvironment(globals)
      ) {
        return (
          <Tooltip side="right" tooltipContentChildren="Metrics">
            <li>
              <Link
                className={
                  currentActiveBlock === moduleName
                    ? styles.navSideBarActive
                    : ''
                }
                to={appPrefix + relativeModulePath}
              >
                <div className={styles.iconCenter}>
                  <FaChartLine title="Monitoring" aria-hidden="true" />
                </div>
                <UpperCase>Monitoring</UpperCase>
              </Link>
            </li>
          </Tooltip>
        );
      }
      return null;
    };

    const isOnboardingWizardEnabled =
      growthExperimentsClient.getAllExperimentConfig().find(e => {
        return e.experiment === 'console_onboarding_wizard_v1';
      })?.status === 'enabled';

    return (
      <div className={styles.container}>
        <div className={styles.flexRow}>
          <div className={styles.sidebar}>
            <div className={styles.header_logo_wrapper}>
              <div className={styles.logoParent}>
                <div className={styles.logo}>
                  <Link
                    to={accessState.hasGraphQLAccess ? '/' : '/access-denied'}
                  >
                    <img className="img img-responsive" src={logo} />
                  </Link>
                </div>
                <Link
                  to={accessState.hasGraphQLAccess ? '/' : '/access-denied'}
                >
                  <div className={styles.project_version}>{serverVersion}</div>
                </Link>
              </div>
            </div>
            <div className={styles.header_items}>
              <ul className={styles.sidebarItems}>
                {renderGraphQLTab()}
                {renderDataTab()}
                {renderActionsTab()}
                {renderRemoteTab()}
                {renderEventsTab()}
                {renderMetricsTab()}
              </ul>
            </div>
            <div
              id="dropdown_wrapper"
              className={`${styles.clusterInfoWrapper} ${
                this.state.isDropdownOpen ? 'open' : ''
              }`}
            >
              {getAdminSecretSection()}
              {renderProjectInfo()}
              {renderMetadataIcon()}
              <a
                id="help"
                href={'https://hasura.io/help'}
                target="_blank"
                rel="noopener noreferrer"
              >
                <div className={styles.headerRightNavbarBtn}>HELP</div>
              </a>
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

          <div className={styles.main + ' container-fluid'}>
            {getMainContent()}
            {serverVersion &&
            accessState &&
            'hasDataAccess' in accessState &&
            !isOnboardingWizardEnabled &&
            accessState.hasDataAccess ? (
              <Onboarding
                dispatch={this.props.dispatch}
                console_opts={this.props.console_opts}
                metadata={metadata?.metadataObject}
              />
            ) : null}
          </div>
          <OnboardingWizard growthExperimentsClient={growthExperimentsClient} />
        </div>
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
  };
};

export default connect(mapStateToProps)(Main);
