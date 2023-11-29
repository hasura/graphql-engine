import React from 'react';
import clsx from 'clsx';

import {
  FaCloud,
  FaCog,
  FaCogs,
  FaDatabase,
  FaInfoCircle,
  FaExclamationCircle,
  FaExclamationTriangle,
  FaFlask,
  FaPlug,
} from 'react-icons/fa';
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
import styles from './Main.module.scss';
import NotificationSection from './NotificationSection';
import * as tooltips from './Tooltips';
import {
  getLoveConsentState,
  getProClickState,
  getUserType,
  setLoveConsentState,
  setProClickState,
} from './utils';
import HeaderNavItem, {
  linkStyle,
  itemContainerStyle,
  activeLinkStyle,
} from './HeaderNavItem';
import { Tooltip } from './../../new-components/Tooltip';
import { Badge } from './../../new-components/Badge';
import { ConsoleDevTools } from '../../utils/console-dev-tools/ConsoleDevTools';
import { isFeatureFlagEnabled } from '../../features/FeatureFlags/hooks/useFeatureFlags';
import { availableFeatureFlagIds } from '../../features/FeatureFlags';

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
/*
const getSettingsSelectedMarker = pathname => {
  const currentActiveBlock = getPathRoot(pathname);

  if (currentActiveBlock === 'settings') {
    return <span className={styles.selected} />;
  }

  return null;
};
 */

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
      this.props.dispatch(_push(`/settings/metadata-status`));
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
      console_opts,
      currentSchema,
      currentSource,
      dispatch,
      metadata,
      migrationModeProgress,
      pathname,
      schemaList,
      serverVersion,
    } = this.props;

    const { isPopUpOpen } = this.state;

    const appPrefix = '';

    const getDataPath = () => {
      const perfMode = isFeatureFlagEnabled(
        availableFeatureFlagIds.performanceMode
      );

      if (perfMode) return '/data';

      return currentSource
        ? schemaList.length
          ? getSchemaBaseRoute(currentSchema, currentSource)
          : getDataSourceBaseRoute(currentSource)
        : '/data';
    };

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

    const getMetadataStatusIcon = pathname => {
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

    const currentActiveBlock = getPathRoot(pathname);

    return (
      <div className={styles.container}>
        <Onboarding
          dispatch={dispatch}
          console_opts={console_opts}
          metadata={metadata.metadataObject}
        />
        <div className={styles.flexRow}>
          <div className="font-sans bg-slate-700 text-slate-100 flex h-16">
            <div className="flex gap-1 flex-grow">
              <div className="px-5 py-2 flex items-center gap-3">
                <Link to="/">
                  <img className="w-24" src={logo} alt="" />
                </Link>
                <Link to="/">
                  <div className="text-white text-xs max-w-[128px]">
                    {serverVersion}
                  </div>
                </Link>
              </div>
              <ul className="flex gap-2" data-testid="Nav bar">
                <HeaderNavItem
                  title="API"
                  icon={<FaFlask aria-hidden="true" />}
                  tooltipText={tooltips.apiExplorer}
                  path="/api/api-explorer"
                  appPrefix={appPrefix}
                  pathname={pathname}
                  isDefault
                />
                <HeaderNavItem
                  title="Data"
                  icon={<FaDatabase aria-hidden="true" />}
                  tooltipText={tooltips.data}
                  path={getDataPath()}
                  appPrefix={appPrefix}
                  pathname={pathname}
                />
                <HeaderNavItem
                  title="Actions"
                  icon={<FaCogs aria-hidden="true" />}
                  tooltipText={tooltips.actions}
                  path="/actions/manage/actions"
                  appPrefix={appPrefix}
                  pathname={pathname}
                />
                <HeaderNavItem
                  title="Remote Schemas"
                  icon={<FaPlug aria-hidden="true" />}
                  tooltipText={tooltips.remoteSchema}
                  path="/remote-schemas/manage/schemas"
                  appPrefix={appPrefix}
                  pathname={pathname}
                />
                <HeaderNavItem
                  title="Events"
                  icon={<FaCloud aria-hidden="true" />}
                  tooltipText={tooltips.events}
                  path="/events/data/manage"
                  appPrefix={appPrefix}
                  pathname={pathname}
                />
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
                <div className={itemContainerStyle}>
                  <Link
                    className={clsx(
                      linkStyle,
                      currentActiveBlock === 'settings' && activeLinkStyle
                    )}
                    to="/settings"
                  >
                    <span className="text-sm self-baseline">
                      {getMetadataStatusIcon()}
                    </span>
                    <span className="uppercase text-left">Settings</span>
                  </Link>
                </div>
                {/* Compensate legacy styles directly with style attribute */}
                <div className={styles.proWrapper} style={{ padding: '0' }}>
                  <div className={itemContainerStyle}>
                    <div
                      className={clsx(
                        linkStyle,
                        isPopUpOpen && activeLinkStyle
                      )}
                      onClick={this.onProIconClick}
                    >
                      <span className="text-sm self-baseline">
                        <FaInfoCircle />
                      </span>
                      <span className="uppercase text-left">CLOUD</span>
                    </div>
                  </div>
                  {isPopUpOpen && <ProPopup toggleOpen={this.toggleProPopup} />}
                </div>

                <Help isSelected={currentActiveBlock === 'support'} />
                <NotificationSection
                  isDropDownOpen={this.state.isDropdownOpen}
                  closeDropDown={this.closeDropDown}
                  toggleDropDown={this.toggleDropDown}
                />
                {!this.state.loveConsentState.isDismissed ? (
                  <div className="bootstrap-jail">
                    <div
                      id="dropdown_wrapper"
                      className={`self-stretch h-full ${
                        this.state.isLoveSectionOpen ? 'open' : ''
                      }`}
                    >
                      <LoveSection
                        closeLoveSection={this.closeLoveSection}
                        toggleLoveSection={this.toggleLoveSection}
                      />
                    </div>
                  </div>
                ) : null}
              </div>
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
        <ConsoleDevTools />
      </div>
    );
  }
}

const mapStateToProps = (state, ownProps) => {
  return {
    ...state.main,
    header: state.header,
    currentSchema: state.tables.currentSchema,
    currentSource: state.tables.currentDataSource,
    metadata: state.metadata,
    console_opts: state.telemetry.console_opts,
    requestHeaders: state.tables.dataHeaders,
    schemaList: state.tables.schemaList,
    pathname: ownProps.location.pathname,
    inconsistentInheritedRole:
      state.tables.modify.permissionsState.inconsistentInhertiedRole,
  };
};

export default connect(mapStateToProps)(Main);
