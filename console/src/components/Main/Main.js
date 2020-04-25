import React from 'react';
import { connect } from 'react-redux';
import { Link } from 'react-router';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';

import * as tooltips from './Tooltips';
import globals from '../../Globals';
import { getPathRoot } from '../Common/utils/urlUtils';

import Spinner from '../Common/Spinner/Spinner';
import WarningSymbol from '../Common/WarningSymbol/WarningSymbol';
import LoveSection from './LoveSection';

import {
  loadServerVersion,
  fetchServerConfig,
  loadLatestServerVersion,
  featureCompatibilityInit,
  emitProClickedEvent,
} from './Actions';

import {
  loadInconsistentObjects,
  redirectToMetadataStatus,
} from '../Services/Settings/Actions';

import { getProClickState, setProClickState } from './utils';
import { checkStableVersion, versionGT } from '../../helpers/versionUtils';
import { getSchemaBaseRoute } from '../Common/utils/routesUtils';
import {
  getLocalStorageItem,
  LS_VERSION_UPDATE_CHECK_LAST_CLOSED,
  setLocalStorageItem,
} from '../Common/utils/localStorageUtils';
import ToolTip from '../Common/Tooltip/Tooltip';
import { setPreReleaseNotificationOptOutInDB } from '../../telemetry/Actions';
import styles from './Main.scss';

class Main extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      updateNotificationVersion: null,
      proClickState: getProClickState(),
      isPopUpOpen: false,
    };

    this.handleBodyClick = this.handleBodyClick.bind(this);
  }

  componentDidMount() {
    const { dispatch } = this.props;

    document
      .querySelector('body')
      .addEventListener('click', this.handleBodyClick);

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
    });

    dispatch(fetchServerConfig());
  }

  toggleProPopup() {
    const { dispatch } = this.props;
    dispatch(emitProClickedEvent({ open: !this.state.isPopUpOpen }));
    this.setState({ isPopUpOpen: !this.state.isPopUpOpen });
  }

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
      const lastUpdateCheckClosed = getLocalStorageItem(
        LS_VERSION_UPDATE_CHECK_LAST_CLOSED
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

  handleBodyClick(e) {
    const heartDropDown = document.getElementById('dropdown_wrapper');
    const heartDropDownOpen = document.querySelectorAll(
      '#dropdown_wrapper.open'
    );

    if (
      heartDropDown &&
      !heartDropDown.contains(e.target) &&
      heartDropDownOpen.length !== 0
    ) {
      heartDropDown.classList.remove('open');
    }
  }

  handleMetadataRedirect() {
    if (this.props.metadata.inconsistentObjects.length > 0) {
      this.props.dispatch(redirectToMetadataStatus());
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

  clickProIcon() {
    this.updateLocalStorageState();
    this.toggleProPopup();
  }

  closeUpdateBanner() {
    const { updateNotificationVersion } = this.state;
    setLocalStorageItem(
      LS_VERSION_UPDATE_CHECK_LAST_CLOSED,
      updateNotificationVersion
    );
    this.setState({ updateNotificationVersion: null });
  }

  render() {
    const {
      children,
      location,
      migrationModeProgress,
      currentSchema,
      serverVersion,
      metadata,
      dispatch,
    } = this.props;

    const { isProClicked } = this.state.proClickState;

    const appPrefix = '';

    const logo = require('./images/white-logo.svg');
    const github = require('./images/Github.svg');
    const discord = require('./images/Discord.svg');
    const mail = require('./images/mail.svg');
    const docs = require('./images/docs-logo.svg');
    const about = require('./images/console-logo.svg');
    const close = require('./images/x-circle.svg');
    const monitoring = require('./images/monitoring.svg');
    const rate = require('./images/rate.svg');
    const regression = require('./images/regression.svg');
    const management = require('./images/management.svg');
    const allow = require('./images/allow.svg');
    const arrowForwardRed = require('./images/arrow_forward-red.svg');
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
              href="https://hasura.io/docs/1.0/graphql/manual/deployment/securing-graphql-endpoint.html"
              target="_blank"
              rel="noopener noreferrer"
            >
              <WarningSymbol
                tooltipText={tooltips.secureEndpoint}
                tooltipPlacement={'left'}
                customStyle={styles.secureSectionSymbol}
              />
              &nbsp;Secure your endpoint
            </a>
          </div>
        );
      }
      return adminSecretHtml;
    };

    const getUpdateNotification = () => {
      let updateNotificationHtml = null;

      const { updateNotificationVersion } = this.state;

      const isStableRelease = checkStableVersion(updateNotificationVersion);

      const getPreReleaseNote = () => {
        const handlePreRelNotifOptOut = e => {
          e.preventDefault();
          e.stopPropagation();

          this.closeUpdateBanner();

          dispatch(setPreReleaseNotificationOptOutInDB());
        };

        return (
          <React.Fragment>
            <span className={styles.middot}> &middot; </span>
            <i>
              This is a pre-release version. Not recommended for production use.
              <span className={styles.middot}> &middot; </span>
              <a href={'#'} onClick={handlePreRelNotifOptOut}>
                Opt out of pre-release notifications
              </a>
              <ToolTip
                message={'Only be notified about stable releases'}
                placement={'top'}
              />
            </i>
          </React.Fragment>
        );
      };

      if (updateNotificationVersion) {
        updateNotificationHtml = (
          <div>
            <div className={styles.phantom} />{' '}
            {/* phantom div to prevent overlapping of banner with content. */}
            <div className={styles.updateBannerWrapper}>
              <div className={styles.updateBanner}>
                <span> Hey there! A new server version </span>
                <span className={styles.versionUpdateText}>
                  {' '}
                  {updateNotificationVersion}
                </span>
                <span> is available </span>
                <span className={styles.middot}> &middot; </span>
                <a
                  href={
                    'https://github.com/hasura/graphql-engine/releases/tag/' +
                    updateNotificationVersion
                  }
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  <span>View Changelog</span>
                </a>
                <span className={styles.middot}> &middot; </span>
                <a
                  className={styles.updateLink}
                  href="https://hasura.io/docs/1.0/graphql/manual/deployment/updating.html"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  <span>Update Now</span>
                </a>
                {!isStableRelease && getPreReleaseNote()}
                <span
                  className={styles.updateBannerClose}
                  onClick={this.closeUpdateBanner.bind(this)}
                >
                  <i className={'fa fa-times'} />
                </span>
              </div>
            </div>
          </div>
        );
      }

      return updateNotificationHtml;
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

    const renderProPopup = () => {
      const { isPopUpOpen } = this.state;
      if (isPopUpOpen) {
        return (
          <div className={styles.proPopUpWrapper}>
            <div className={styles.popUpHeader}>
              Hasura <span>PRO</span>
              <img
                onClick={this.toggleProPopup.bind(this)}
                className={styles.popUpClose}
                src={close}
                alt={'Close'}
              />
            </div>
            <div className={styles.popUpBodyWrapper}>
              <div className={styles.featuresDescription}>
                Hasura Pro is an enterprise-ready version of Hasura that comes
                with the following features:
              </div>
              <div className={styles.proFeaturesList}>
                <div className={styles.featuresImg}>
                  <img src={monitoring} alt={'Monitoring'} />
                </div>
                <div className={styles.featuresList}>
                  <div className={styles.featuresTitle}>
                    Monitoring/Analytics
                  </div>
                  <div className={styles.featuresDescription}>
                    Complete observability to troubleshoot errors and drill-down
                    into individual operations.
                  </div>
                </div>
              </div>
              <div className={styles.proFeaturesList}>
                <div className={styles.featuresImg}>
                  <img src={rate} alt={'Rate'} />
                </div>
                <div className={styles.featuresList}>
                  <div className={styles.featuresTitle}>Rate Limiting</div>
                  <div className={styles.featuresDescription}>
                    Prevent abuse with role-based rate limits.
                  </div>
                </div>
              </div>
              <div className={styles.proFeaturesList}>
                <div className={styles.featuresImg}>
                  <img src={regression} alt={'Regression'} />
                </div>
                <div className={styles.featuresList}>
                  <div className={styles.featuresTitle}>Regression Testing</div>
                  <div className={styles.featuresDescription}>
                    Automatically create regression suites to prevent breaking
                    changes.
                  </div>
                </div>
              </div>
              <div className={styles.proFeaturesList}>
                <div className={styles.featuresImg}>
                  <img src={management} alt={'Management'} />
                </div>
                <div className={styles.featuresList}>
                  <div className={styles.featuresTitle}>Team Management</div>
                  <div className={styles.featuresDescription}>
                    Login to a Hasura project with granular privileges.
                  </div>
                </div>
              </div>
              <div className={styles.proFeaturesList}>
                <div className={styles.featuresImg}>
                  <img src={allow} alt={'allow'} />
                </div>
                <div className={styles.featuresList}>
                  <div className={styles.featuresTitle}>
                    Allow Listing Workflows
                  </div>
                  <div className={styles.featuresDescription}>
                    Setup allow lists across dev, staging and production
                    environments with easy workflows.
                  </div>
                </div>
              </div>
            </div>
            <div className={styles.popUpFooter}>
              <a
                href={
                  'https://hasura.io/getintouch?type=hasuraprodemo&utm_source=console'
                }
                target={'_blank'}
                rel="noopener noreferrer"
              >
                Set up a chat to learn more{' '}
                <img
                  className={styles.arrow}
                  src={arrowForwardRed}
                  alt={'Arrow'}
                />
              </a>
            </div>
          </div>
        );
      }
      return null;
    };

    return (
      <div className={styles.container}>
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
                  'GraphiQL',
                  'fa-flask',
                  tooltips.apiExplorer,
                  '/api-explorer',
                  true
                )}
                {getSidebarItem(
                  'Data',
                  'fa-database',
                  tooltips.data,
                  getSchemaBaseRoute(currentSchema)
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
                )}
                {getSidebarItem(
                  'Events',
                  'fa-cloud',
                  tooltips.events,
                  '/events/manage/triggers'
                )}
              </ul>
            </div>
            <div id="dropdown_wrapper" className={styles.clusterInfoWrapper}>
              {getAdminSecretSection()}
              <div className={styles.helpSection + ' ' + styles.proWrapper}>
                <span
                  className={
                    !isProClicked ? styles.proName : styles.proNameClicked
                  }
                  onClick={this.clickProIcon.bind(this)}
                >
                  PRO
                </span>
                {renderProPopup()}
              </div>
              <Link to="/settings">
                <div className={styles.helpSection + ' ' + styles.settingsIcon}>
                  {getMetadataStatusIcon()}
                  {getSettingsSelectedMarker()}
                </div>
              </Link>
              <div className={styles.supportSection}>
                <div
                  id="help"
                  className={styles.helpSection + ' dropdown-toggle'}
                  data-toggle="dropdown"
                  aria-expanded="false"
                  aria-haspopup="true"
                >
                  <i className={styles.question + ' fa fa-question'} />
                </div>
                <ul
                  className={'dropdown-menu ' + styles.help_dropdown_menu}
                  aria-labelledby="help"
                >
                  <div className={styles.help_dropdown_menu_container}>
                    <li className={'dropdown-item'}>
                      <a
                        href="https://github.com/hasura/graphql-engine/issues"
                        target="_blank"
                        rel="noopener noreferrer"
                      >
                        <img
                          className={'img-responsive'}
                          src={github}
                          alt={'github'}
                        />
                        <span>Report bugs & suggest improvements</span>
                      </a>
                    </li>
                    <li className={'dropdown-item'}>
                      <a
                        href="https://discordapp.com/invite/vBPpJkS"
                        target="_blank"
                        rel="noopener noreferrer"
                      >
                        <img
                          className={'img-responsive'}
                          src={discord}
                          alt={'discord'}
                        />
                        <span>Join discord community forum</span>
                      </a>
                    </li>
                    <li className={'dropdown-item'}>
                      <a href="mailto:support@hasura.io">
                        <img
                          className={'img-responsive'}
                          src={mail}
                          alt={'mail'}
                        />
                        <span>Reach out ({'support@hasura.io'})</span>
                      </a>
                    </li>
                    <li className={'dropdown-item'}>
                      <a
                        href="https://hasura.io/docs/"
                        target="_blank"
                        rel="noopener noreferrer"
                      >
                        <img
                          className={'img-responsive'}
                          src={docs}
                          alt={'docs'}
                        />
                        <span>Head to docs</span>
                      </a>
                    </li>
                    <li className={'dropdown-item'}>
                      <Link to="/about">
                        <img
                          className={'img-responsive'}
                          src={about}
                          alt={'about'}
                        />
                        <span>About</span>
                      </Link>
                    </li>
                  </div>
                </ul>
              </div>
              <LoveSection />
            </div>
          </div>

          <div className={styles.main + ' container-fluid'}>
            {getMainContent()}
          </div>

          {getUpdateNotification()}
        </div>
      </div>
    );
  }
}

const mapStateToProps = (state, ownProps) => {
  return {
    ...state.main,
    header: { ...state.header },
    pathname: ownProps.location.pathname,
    currentSchema: state.tables.currentSchema,
    metadata: state.metadata,
    console_opts: state.telemetry.console_opts,
  };
};

export default connect(mapStateToProps)(Main);
