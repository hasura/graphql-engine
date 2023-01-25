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

import HeaderNavItem from './HeaderNavItem';

import Dropdown from 'react-bootstrap/lib/Dropdown';
import MenuItem from 'react-bootstrap/lib/MenuItem';

import globals from '../../Globals';
import 'react-toggle/style.css';
import { Spinner } from '@hasura/console-oss';
import {
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
import projectImg from './images/project.svg';
import EELogo from './images/hasura-ee-mono-light.svg';

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

  /**
   * checks if current project is an enterprise project
   * NOTE: an enterprise project is any non free plan project
   * created by a Hasura enterprise user
   */
  isEnterpriseProject() {
    return (
      (this.props?.project?.is_enterprise_user &&
        this.props?.project?.plan_name !== 'cloud_free') ||
      globals.consoleType === 'pro-lite'
    );
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

    const getMetadataSelectedMarker = () => {
      let metadataSelectedMarker = null;

      if (currentActiveBlock === 'settings') {
        metadataSelectedMarker = <span className={styles.selected} />;
      }

      return metadataSelectedMarker;
    };

    const getMetadataIcon = () => {
      if (metadata.inconsistentObjects.length === 0) {
        return <FaCog className={styles.question} />;
      }

      return (
        <div className={styles.question}>
          <FaCog />
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
                href="https://hasura.io/docs/latest/deployment/securing-graphql-endpoint/"
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

    const getDataPath = () => {
      if (currentSource && currentSchema) {
        return `data/${currentSource}/schema/${currentSchema}`;
      }
      return 'data/';
    };

    const renderMetricsTab = () => {
      if (
        'hasMetricAccess' in accessState &&
        accessState.hasMetricAccess &&
        isMonitoringTabSupportedEnvironment(globals)
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

    const getLogoSrc = () => {
      if (this.isEnterpriseProject()) {
        return EELogo;
      }

      return logo;
    };

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
                    <img className="img img-responsive" src={getLogoSrc()} />
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
