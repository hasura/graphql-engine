import React from 'react';
import { connect } from 'react-redux';
import { Link } from 'react-router';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import globals from '../../Globals';
import * as tooltip from './Tooltips';
import 'react-toggle/style.css';
import Spinner from '../Common/Spinner/Spinner';
import {
  loadServerVersion,
  fetchServerConfig,
  loadLatestServerVersion,
  featureCompatibilityInit,
} from './Actions';
import { loadConsoleTelemetryOpts } from '../../telemetry/Actions.js';
import './NotificationOverrides.css';
import {
  loadInconsistentObjects,
  redirectToMetadataStatus,
} from '../Services/Metadata/Actions';

import {
  getLoveConsentState,
  setLoveConsentState,
} from './loveConsentLocalStorage';

import { versionGT, FT_JWT_ANALYZER } from '../../helpers/versionUtils';

class Main extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      showUpdateNotification: false,
      loveConsentState: getLoveConsentState(),
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

      dispatch(loadInconsistentObjects()).then(() => {
        this.handleMetadataRedirect();
      });

      dispatch(loadConsoleTelemetryOpts());

      dispatch(loadLatestServerVersion()).then(() => {
        this.setShowUpdateNotification();
      });
    });
  }

  componentWillReceiveProps(nextProps) {
    const {
      [FT_JWT_ANALYZER]: currJwtAnalyzerCompatibility,
    } = this.props.featuresCompatibility;
    const {
      [FT_JWT_ANALYZER]: nextJwtAnalyzerCompatibility,
    } = nextProps.featuresCompatibility;

    if (
      currJwtAnalyzerCompatibility !== nextJwtAnalyzerCompatibility &&
      nextJwtAnalyzerCompatibility
    ) {
      this.fetchServerConfig();
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

  fetchServerConfig() {
    const { dispatch } = this.props;

    dispatch(fetchServerConfig());
  }

  handleBodyClick(e) {
    const heartDropDownOpen = document.querySelectorAll(
      '#dropdown_wrapper.open'
    );
    if (
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

  render() {
    const {
      children,
      location,
      migrationModeProgress,
      currentSchema,
      serverVersion,
      latestServerVersion,
      metadata,
    } = this.props;

    const styles = require('./Main.scss');

    const appPrefix = '';

    const logo = require('./images/white-logo.svg');
    const github = require('./images/Github.svg');
    const discord = require('./images/Discord.svg');
    const mail = require('./images/mail.svg');
    const docs = require('./images/logo.svg');
    const pixHeart = require('./images/pix-heart.svg');

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

      if (currentActiveBlock === 'metadata') {
        metadataSelectedMarker = <span className={styles.selected} />;
      }

      return metadataSelectedMarker;
    };

    const getMetadataIcon = () => {
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

      if (
        !globals.isAdminSecretSet &&
        (globals.adminSecret === '' || globals.adminSecret === null)
      ) {
        adminSecretHtml = (
          <div className={styles.secureSection}>
            <OverlayTrigger placement="left" overlay={tooltip.secureEndpoint}>
              <a href="https://docs.hasura.io/1.0/graphql/manual/deployment/securing-graphql-endpoint.html">
                <i
                  className={
                    styles.padd_small_right + ' fa fa-exclamation-triangle'
                  }
                />
                Secure your endpoint
              </a>
            </OverlayTrigger>
          </div>
        );
      }
      return adminSecretHtml;
    };

    const getUpdateNotification = () => {
      let updateNotificationHtml = null;

      if (this.state.showUpdateNotification) {
        updateNotificationHtml = (
          <div>
            <div className={styles.phantom} />{' '}
            {/* phantom div to prevent overlapping of banner with content. */}
            <div className={styles.updateBannerWrapper}>
              <div className={styles.updateBanner}>
                <span> Hey there! A new server version </span>
                <span className={styles.versionUpdateText}>
                  {' '}
                  {latestServerVersion}
                </span>
                <span> is available </span>
                <span className={styles.middot}> &middot; </span>
                <a
                  href={
                    'https://github.com/hasura/graphql-engine/releases/tag/' +
                    latestServerVersion
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
                <OverlayTrigger placement="right" overlay={tooltip.apiexplorer}>
                  <li>
                    <Link
                      className={
                        currentActiveBlock === 'api-explorer' ||
                        currentActiveBlock === ''
                          ? styles.navSideBarActive
                          : ''
                      }
                      to={appPrefix + '/api-explorer'}
                    >
                      <div
                        className={styles.iconCenter}
                        data-test="api-explorer"
                      >
                        <i
                          title="API Explorer"
                          className="fa fa-flask"
                          aria-hidden="true"
                        />
                      </div>
                      <p>GraphiQL</p>
                    </Link>
                  </li>
                </OverlayTrigger>
                <OverlayTrigger placement="right" overlay={tooltip.data}>
                  <li>
                    <Link
                      className={
                        currentActiveBlock === 'data'
                          ? styles.navSideBarActive
                          : ''
                      }
                      to={appPrefix + '/data/schema/' + currentSchema}
                    >
                      <div className={styles.iconCenter}>
                        <i
                          title="Data Service"
                          className="fa fa-database"
                          aria-hidden="true"
                        />
                      </div>
                      <p>Data</p>
                    </Link>
                  </li>
                </OverlayTrigger>
                <OverlayTrigger
                  placement="right"
                  overlay={tooltip.customresolver}
                >
                  <li>
                    <Link
                      className={
                        currentActiveBlock === 'remote-schemas'
                          ? styles.navSideBarActive
                          : ''
                      }
                      to={appPrefix + '/remote-schemas/manage/schemas'}
                    >
                      <div className={styles.iconCenter}>
                        <i
                          title="Remote Schemas"
                          className="fa fa-plug"
                          aria-hidden="true"
                        />
                      </div>
                      <p>Remote Schemas</p>
                    </Link>
                  </li>
                </OverlayTrigger>
                <OverlayTrigger placement="right" overlay={tooltip.events}>
                  <li>
                    <Link
                      className={
                        currentActiveBlock === 'events'
                          ? styles.navSideBarActive
                          : ''
                      }
                      to={appPrefix + '/events/manage/triggers'}
                    >
                      <div className={styles.iconCenter}>
                        <i
                          title="Events"
                          className="fa fa-cloud"
                          aria-hidden="true"
                        />
                      </div>
                      <p>Events</p>
                    </Link>
                  </li>
                </OverlayTrigger>
              </ul>
            </div>
            <div id="dropdown_wrapper" className={styles.clusterInfoWrapper}>
              {getAdminSecretSection()}

              <Link to="/metadata">
                <div className={styles.helpSection + ' ' + styles.settingsIcon}>
                  {getMetadataIcon()}
                  {getMetadataSelectedMarker()}
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
                  className={
                    'dropdown-menu ' +
                    styles.help_dropdown_menu +
                    ' ' +
                    getHelpDropdownPosStyle()
                  }
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
                        href="https://docs.hasura.io/"
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
                  </div>
                </ul>
              </div>

              {getLoveSection()}
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
  };
};

export default connect(mapStateToProps)(Main);
