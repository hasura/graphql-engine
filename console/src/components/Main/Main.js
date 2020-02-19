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
import logo from './images/white-logo.svg';

import NotificationSection from './NotificationSection';

import styles from './Main.scss';

import {
  loadServerVersion,
  fetchServerConfig,
  loadLatestServerVersion,
  featureCompatibilityInit,
  emitProClickedEvent,
  fetchPostgresVersion,
  fetchConsoleNotifications,
} from './Actions';

import {
  loadInconsistentObjects,
  redirectToMetadataStatus,
} from '../Services/Settings/Actions';

import {
  getProClickState,
  setProClickState,
  getLoveConsentState,
  setLoveConsentState,
  getUserType,
} from './utils';
<<<<<<< HEAD
import { getSchemaBaseRoute } from '../Common/utils/routesUtils';
import LoveSection from './LoveSection';
import { Help, ProPopup } from './components/';
import { HASURA_COLLABORATOR_TOKEN } from '../../constants';
import { UPDATE_CONSOLE_NOTIFICATIONS } from '../../telemetry/Actions';

const updateRequestHeaders = props => {
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
=======

import { checkStableVersion, versionGT } from '../../helpers/versionUtils';
import { getSchemaBaseRoute } from '../Common/utils/routesUtils';
import {
  getLocalStorageItem,
  LS_VERSION_UPDATE_CHECK_LAST_CLOSED,
  setLocalStorageItem,
} from '../Common/utils/localStorageUtils';
import ToolTip from '../Common/Tooltip/Tooltip';
import { setPreReleaseNotificationOptOutInDB } from '../../telemetry/Actions';
>>>>>>> 20d9cd158 (v1.1.0-beta.3 console fixes (#3905))

class Main extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      updateNotificationVersion: null,
<<<<<<< HEAD
=======
      loveConsentState: getLoveConsentState(),
>>>>>>> 20d9cd158 (v1.1.0-beta.3 console fixes (#3905))
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

<<<<<<< HEAD
      dispatch(loadInconsistentObjects({ shouldReloadMetadata: false })).then(
        () => {
          this.handleMetadataRedirect();
        }
      );

      dispatch(loadLatestServerVersion());
      dispatch(fetchConsoleNotifications());
    });

    dispatch(fetchPostgresVersion);
    dispatch(fetchServerConfig);
=======
      dispatch(loadInconsistentObjects()).then(() => {
        this.handleMetadataRedirect();
      });

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
      latestServerVersion,
      serverVersion,
      console_opts,
    } = this.props;

    const allowPreReleaseNotifications =
      !console_opts || !console_opts.disablePreReleaseUpdateNotifications;

    const latestServerVersionToCheck = allowPreReleaseNotifications
      ? latestServerVersion
      : latestStableServerVersion;

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
>>>>>>> 20d9cd158 (v1.1.0-beta.3 console fixes (#3905))
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
  }

<<<<<<< HEAD
=======
  closeLoveIcon() {
    const s = {
      isDismissed: true,
    };
    setLoveConsentState(s);
    this.setState({
      loveConsentState: { ...getLoveConsentState() },
    });
  }

>>>>>>> 20d9cd158 (v1.1.0-beta.3 console fixes (#3905))
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
<<<<<<< HEAD
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
=======
  }

  closeUpdateBanner() {
    const { updateNotificationVersion } = this.state;
    setLocalStorageItem(
      LS_VERSION_UPDATE_CHECK_LAST_CLOSED,
      updateNotificationVersion
    );
    this.setState({ updateNotificationVersion: null });
  }
>>>>>>> 20d9cd158 (v1.1.0-beta.3 console fixes (#3905))

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
              href="https://hasura.io/docs/1.0/graphql/manual/deployment/securing-graphql-endpoint.html"
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

<<<<<<< HEAD
=======
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
              <ToolTip message={'Only be notified about stable releases'} />
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
                  href="https://docs.hasura.io/1.0/graphql/manual/deployment/updating.html"
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

    const getLoveSection = () => {
      let loveSectionHtml = null;

      if (!this.state.loveConsentState.isDismissed) {
        loveSectionHtml = [
          <div
            key="main_love_1"
            className={styles.shareSection + ' dropdown-toggle'}
            aria-expanded="false"
            onClick={this.handleDropdownToggle.bind(this)}
          >
            <img
              className={'img-responsive'}
              src={pixHeart}
              alt={'pix Heart'}
            />
            {/* <i className={styles.heart + ' fa fa-heart'} /> */}
          </div>,
          <ul
            key="main_love_2"
            className={'dropdown-menu ' + styles.dropdown_menu}
          >
            <div className={styles.dropdown_menu_container}>
              <div className={styles.closeDropDown}>
                <i
                  className="fa fa-close"
                  onClick={this.closeLoveIcon.bind(this)}
                />
                {/*
                        <img
                          className={'img-responsive'}
                          src={closeIcon}
                          alt={'closeIcon'}
                          onClick={this.closeLoveIcon.bind(this)}
                        />
                        */}
              </div>
              {/*
                      <div className={styles.arrow_up_dropdown} />
                      <div className={styles.graphqlHeartText}>
                        Love GraphQL Engine? Shout it from the rooftops!
                        <br />
                        Or just spread the word{' '}
                        <span role="img" aria-label="smile">
                          😊
                        </span>
                      </div>
                      */}
              <div className={styles.displayFlex}>
                <li className={styles.pixelText1}>
                  Roses are red, <br />
                  Violets are blue;
                  <br />
                  Star us on GitHub,
                  <br />
                  To make our <i className={'fa fa-heart'} /> go wooooo!
                </li>
                <li className={'dropdown-item'}>
                  <a
                    href="https://github.com/hasura/graphql-engine"
                    target="_blank"
                    rel="noopener noreferrer"
                  >
                    <div className={styles.socialIcon}>
                      <img
                        className="img img-responsive"
                        src={`${globals.assetsPath}/common/img/githubicon.png`}
                        alt={'GitHub'}
                      />
                    </div>
                    <div className={styles.pixelText}>
                      <i className="fa fa-star" />
                      &nbsp; Star
                    </div>
                  </a>
                  {/*
                          <div className={styles.gitHubBtn}>
                            <iframe
                              title="github"
                              src="https://ghbtns.com/github-btn.html?user=hasura&repo=graphql-engine&type=star&count=true"
                              frameBorder="0"
                              scrolling="0"
                              width="100px"
                              height="30px"
                            />
                          </div>
                          */}
                </li>
                <li className={'dropdown-item '}>
                  <a
                    href="https://twitter.com/intent/tweet?hashtags=graphql,postgres&text=Just%20deployed%20a%20GraphQL%20backend%20with%20@HasuraHQ!%20%E2%9D%A4%EF%B8%8F%20%F0%9F%9A%80%0Ahttps://github.com/hasura/graphql-engine%0A"
                    target="_blank"
                    rel="noopener noreferrer"
                  >
                    <div className={styles.socialIcon}>
                      <img
                        className="img img-responsive"
                        src={`${globals.assetsPath}/common/img/twittericon.png`}
                        alt={'Twitter'}
                      />
                    </div>
                    <div className={styles.pixelText}>
                      <i className="fa fa-twitter" />
                      &nbsp; Tweet
                    </div>
                  </a>
                </li>
              </div>
            </div>
          </ul>,
        ];
      }

      return loveSectionHtml;
    };

    const getHelpDropdownPosStyle = () => {
      let helpDropdownPosStyle = '';

      if (this.state.loveConsentState.isDismissed) {
        helpDropdownPosStyle = styles.help_dropdown_menu_heart_dismissed;
      }

      return helpDropdownPosStyle;
    };

>>>>>>> 20d9cd158 (v1.1.0-beta.3 console fixes (#3905))
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

<<<<<<< HEAD
=======
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

>>>>>>> 20d9cd158 (v1.1.0-beta.3 console fixes (#3905))
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
<<<<<<< HEAD
    requestHeaders: state.tables.dataHeaders,
=======
>>>>>>> 20d9cd158 (v1.1.0-beta.3 console fixes (#3905))
  };
};

export default connect(mapStateToProps)(Main);
