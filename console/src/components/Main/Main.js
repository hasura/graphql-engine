import React from 'react';

import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';
import { connect } from 'react-redux';
import { Link } from 'react-router';

import { HASURA_COLLABORATOR_TOKEN } from '../../constants';
import globals from '../../Globals';
import { versionGT } from '../../helpers/versionUtils';
import { loadInconsistentObjects } from '../../metadata/actions';
import { UPDATE_CONSOLE_NOTIFICATIONS } from '../../telemetry/Actions';
import { getLSItem, LS_KEYS, setLSItem } from '../../utils/localStorage';
import Onboarding from '../Common/Onboarding';
import Spinner from '../Common/Spinner/Spinner';
import {
  getSchemaBaseRoute,
  redirectToMetadataStatus,
  getDataSourceBaseRoute,
} from '../Common/utils/routesUtils';
import { getPathRoot } from '../Common/utils/urlUtils';
import WarningSymbol from '../Common/WarningSymbol/WarningSymbol';
import _push from '../Services/Data/push';
import {
  emitProClickedEvent,
  featureCompatibilityInit,
  fetchConsoleNotifications,
  fetchServerConfig,
  loadLatestServerVersion,
  loadServerVersion,
} from './Actions';
import { Help, ProPopup } from './components/';
import { UpdateVersion } from './components/UpdateVersion';
import logo from './images/white-logo.svg';
import LoveSection from './LoveSection';
import styles from './Main.scss';
import NotificationSection from './NotificationSection';
import * as tooltips from './Tooltips';
import {
  getLoveConsentState,
  getProClickState,
  getUserType,
  setLoveConsentState,
  setProClickState,
} from './utils';

export const updateRequestHeaders = props => {
  const { requestHeaders, dispatch } = props;

  const collabTokenKey = Object.keys(requestHeaders).find(
    hdr => hdr.toLowerCase() === HASURA_COLLABORATOR_TOKEN
  );

  if (collabTokenKey) {
    const userID = getUserType(requestHeaders[collabTokenKey]);
    if (props.console_opts && props.console_opts.console_notifications) {
      if (!props.console_opts.console_notifications[userID]) {
        dispatch({
          type: UPDATE_CONSOLE_NOTIFICATIONS,
          data: {
            ...props.console_opts.console_notifications,
            [userID]: {
              read: [],
              date: null,
              showBadge: true,
            },
          },
        });
      }
    }
  }
};

class Main extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      updateNotificationVersion: null,
      proClickState: getProClickState(),
      loveConsentState: getLoveConsentState(),
      isPopUpOpen: false,
      isDropdownOpen: false,
      isLoveSectionOpen: false,
    };
  }

  componentDidMount() {
    const { dispatch } = this.props;
    updateRequestHeaders(this.props);
    dispatch(loadServerVersion()).then(() => {
      dispatch(featureCompatibilityInit());

      dispatch(loadInconsistentObjects({ shouldReloadMetadata: false })).then(
        () => {
          this.handleMetadataRedirect();
        }
      );

      dispatch(loadLatestServerVersion()).then(() => {
        this.setShowUpdateNotification();
      });

      dispatch(fetchConsoleNotifications());
    });

    dispatch(fetchServerConfig);
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

  toggleProPopup = () => {
    const { dispatch } = this.props;
    dispatch(emitProClickedEvent({ open: !this.state.isPopUpOpen }));
    this.setState({ isPopUpOpen: !this.state.isPopUpOpen });
  };

  handleMetadataRedirect() {
    if (this.props.metadata.inconsistentObjects.length > 0) {
      this.props.dispatch(redirectToMetadataStatus());
    }
    if (
      this.props.metadata.inconsistentInheritedRoles.length > 0 &&
      !this.props.inconsistentInheritedRole
    ) {
      this.props.dispatch(
        _push(`${globals.urlPrefix}/settings/metadata-status`)
      );
    }
  }

  updateLocalStorageState() {
    const s = getProClickState();
    if (s && 'isProClicked' in s && !s.isProClicked) {
      setProClickState({
        isProClicked: !s.isProClicked,
      });
      this.setState({
        proClickState: { ...getProClickState() },
      });
    }
  }

  onProIconClick = () => {
    this.updateLocalStorageState();
    this.toggleProPopup();
  };

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

  closeLoveSection = () => {
    this.setState(
      {
        isLoveSectionOpen: false,
      },
      () => {
        setLoveConsentState({ isDismissed: true });
        this.setState({ loveConsentState: { ...getLoveConsentState() } });
      }
    );
  };

  toggleLoveSection = () => {
    this.setState(prevState => ({
      isLoveSectionOpen: !prevState.isLoveSectionOpen,
    }));
  };

  closeUpdateBanner = () => {
    const { updateNotificationVersion } = this.state;
    setLSItem(LS_KEYS.versionUpdateCheckLastClosed, updateNotificationVersion);
    this.setState({ updateNotificationVersion: null });
  };

  setShowUpdateNotification() {
    const {
      latestStableServerVersion,
      latestPreReleaseServerVersion,
      serverVersion,
      console_opts,
    } = this.props;

    const allowPreReleaseNotifications =
      !console_opts || !console_opts.disablePreReleaseUpdateNotifications;

    let latestServerVersionToCheck;
    if (
      allowPreReleaseNotifications &&
      versionGT(latestPreReleaseServerVersion, latestStableServerVersion)
    ) {
      latestServerVersionToCheck = latestPreReleaseServerVersion;
    } else {
      latestServerVersionToCheck = latestStableServerVersion;
    }

    try {
      const lastUpdateCheckClosed = getLSItem(
        LS_KEYS.versionUpdateCheckLastClosed
      );

      if (lastUpdateCheckClosed !== latestServerVersionToCheck) {
        const isUpdateAvailable = versionGT(
          latestServerVersionToCheck,
          serverVersion
        );

        if (isUpdateAvailable) {
          this.setState({
            updateNotificationVersion: latestServerVersionToCheck,
          });
        }
      }
    } catch (e) {
      console.error(e);
    }
  }

  render() {
    const {
      children,
      location,
      migrationModeProgress,
      currentSchema,
      serverVersion,
      metadata,
      console_opts,
      currentSource,
      dispatch,
      schemaList,
    } = this.props;

    const {
      proClickState: { isProClicked },
      isPopUpOpen,
    } = this.state;

    const appPrefix = '';

    const currentLocation = location.pathname;
    const currentActiveBlock = getPathRoot(currentLocation);

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

    const getSettingsSelectedMarker = () => {
      let metadataSelectedMarker = null;

      if (currentActiveBlock === 'settings') {
        metadataSelectedMarker = <span className={styles.selected} />;
      }

      return metadataSelectedMarker;
    };

    const getMetadataStatusIcon = () => {
      if (metadata.inconsistentObjects.length === 0) {
        return <i className={styles.question + ' fa fa-cog'} />;
      }
      return (
        <div className={styles.question}>
          <i className={'fa fa-cog'} />
          <div className={styles.overlappingExclamation}>
            <div className={styles.iconWhiteBackground} />
            <div>
              <i className={'fa fa-exclamation-circle'} />
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
            <a
              href="https://hasura.io/docs/latest/graphql/core/deployment/securing-graphql-endpoint.html"
              target="_blank"
              rel="noopener noreferrer"
            >
              <WarningSymbol
                tooltipText={tooltips.secureEndpoint}
                tooltipPlacement={'left'}
                customStyle={styles.secureSectionSymbol}
              />
              <span className={styles.secureSectionText}>
                &nbsp;Secure your endpoint
              </span>
            </a>
          </div>
        );
      }
      return adminSecretHtml;
    };

    const getSidebarItem = (
      title,
      icon,
      tooltipText,
      path,
      isDefault = false
    ) => {
      const itemTooltip = <Tooltip id={tooltipText}>{tooltipText}</Tooltip>;

      const block = getPathRoot(path);

      return (
        <OverlayTrigger placement="right" overlay={itemTooltip}>
          <li>
            <Link
              className={
                currentActiveBlock === block ||
                (isDefault && currentActiveBlock === '')
                  ? styles.navSideBarActive
                  : ''
              }
              to={appPrefix + path}
            >
              <div className={styles.iconCenter} data-test={block}>
                <i className={`fa ${icon}`} aria-hidden="true" />
              </div>
              <p>{title}</p>
            </Link>
          </li>
        </OverlayTrigger>
      );
    };

    return (
      <div className={styles.container}>
        <Onboarding
          dispatch={dispatch}
          console_opts={console_opts}
          metadata={metadata.metadataObject}
        />
        <div className={styles.flexRow}>
          <div className={styles.sidebar}>
            <div className={styles.header_logo_wrapper}>
              <div className={styles.logoParent}>
                <div className={styles.logo}>
                  <Link to="/">
                    <img className="img img-responsive" src={logo} />
                  </Link>
                </div>
                <Link to="/">
                  <div className={styles.project_version}>{serverVersion}</div>
                </Link>
              </div>
            </div>
            <div className={styles.header_items}>
              <ul className={styles.sidebarItems}>
                {getSidebarItem(
                  'API',
                  'fa-flask',
                  tooltips.apiExplorer,
                  '/api/api-explorer',
                  true
                )}
                {getSidebarItem(
                  'Data',
                  'fa-database',
                  tooltips.data,
                  currentSource
                    ? schemaList.length
                      ? getSchemaBaseRoute(currentSchema, currentSource)
                      : getDataSourceBaseRoute(currentSource)
                    : '/data'
                )}
                {getSidebarItem(
                  'Actions',
                  'fa-cogs',
                  tooltips.actions,
                  '/actions/manage/actions'
                )}
                {getSidebarItem(
                  'Remote Schemas',
                  'fa-plug',
                  tooltips.remoteSchema,
                  '/remote-schemas/manage/schemas'
                )}{' '}
                {getSidebarItem(
                  'Events',
                  'fa-cloud',
                  tooltips.events,
                  '/events/data/manage'
                )}
              </ul>
            </div>
            <div
              id="dropdown_wrapper"
              className={`${styles.clusterInfoWrapper} ${
                this.state.isDropdownOpen ? 'open' : ''
              }`}
            >
              {getAdminSecretSection()}
              <div
                className={`${styles.headerRightNavbarBtn} ${styles.proWrapper}`}
                onClick={this.onProIconClick}
              >
                <span
                  className={`
                    ${isProClicked ? styles.proNameClicked : styles.proName}
                    ${isPopUpOpen ? styles.navActive : ''}`}
                >
                  CLOUD
                </span>
                {isPopUpOpen && <ProPopup toggleOpen={this.toggleProPopup} />}
              </div>
              <Link to="/settings">
                <div className={styles.headerRightNavbarBtn}>
                  {getMetadataStatusIcon()}
                  {getSettingsSelectedMarker()}
                </div>
              </Link>
              <Help isSelected={currentActiveBlock === 'support'} />
              <NotificationSection
                isDropDownOpen={this.state.isDropdownOpen}
                closeDropDown={this.closeDropDown}
                toggleDropDown={this.toggleDropDown}
              />
              {!this.state.loveConsentState.isDismissed ? (
                <div
                  id="dropdown_wrapper"
                  className={`${this.state.isLoveSectionOpen ? 'open' : ''}`}
                >
                  <LoveSection
                    closeLoveSection={this.closeLoveSection}
                    toggleLoveSection={this.toggleLoveSection}
                  />
                </div>
              ) : null}
            </div>
          </div>
          <div className={styles.main + ' container-fluid'}>
            {getMainContent()}
          </div>
          <UpdateVersion
            closeUpdateBanner={this.closeUpdateBanner}
            dispatch={this.props.dispatch}
            updateNotificationVersion={this.state.updateNotificationVersion}
          />
        </div>
      </div>
    );
  }
}

const mapStateToProps = (state, ownProps) => {
  return {
    ...state.main,
    header: state.header,
    pathname: ownProps.location.pathname,
    currentSchema: state.tables.currentSchema,
    currentSource: state.tables.currentDataSource,
    metadata: state.metadata,
    console_opts: state.telemetry.console_opts,
    requestHeaders: state.tables.dataHeaders,
    schemaList: state.tables.schemaList,
    inconsistentInheritedRole:
      state.tables.modify.permissionsState.inconsistentInhertiedRole,
  };
};

export default connect(mapStateToProps)(Main);
