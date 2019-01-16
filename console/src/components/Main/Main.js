import React from 'react';
import { connect } from 'react-redux';
import { Link } from 'react-router';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import globals from '../../Globals';
import * as tooltip from './Tooltips';
import 'react-toggle/style.css';
import Spinner from '../Common/Spinner/Spinner';
import { loadServerVersion, checkServerUpdates } from './Actions';
import './NotificationOverrides.css';
import semverCheck from '../../helpers/semver';

const semver = require('semver');

import {
  getLoveConsentState,
  setLoveConsentState,
} from '../Common/localStorageManager';

class Main extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      showBannerNotification: false,
      showEvents: false,
      showSchemaStitch: false,
    };

    this.state.loveConsentState = getLoveConsentState();
    this.handleBodyClick = this.handleBodyClick.bind(this);
  }
  componentDidMount() {
    const { dispatch } = this.props;
    document
      .querySelector('body')
      .addEventListener('click', this.handleBodyClick);
    dispatch(loadServerVersion()).then(() => {
      dispatch(checkServerUpdates()).then(() => {
        let isUpdateAvailable = false;
        try {
          this.checkEventsTab().then(() => {
            this.checkSchemaStitch();
          });
          isUpdateAvailable = semver.gt(
            this.props.latestServerVersion,
            this.props.serverVersion
          );
          const isClosedBefore = window.localStorage.getItem(
            this.props.latestServerVersion + '_BANNER_NOTIFICATION_CLOSED'
          );
          if (isClosedBefore === 'true') {
            isUpdateAvailable = false;
            this.setState({ showBannerNotification: false });
          } else {
            this.setState({ showBannerNotification: isUpdateAvailable });
          }
        } catch (e) {
          console.error(e);
        }
      });
    });
  }
  checkSchemaStitch() {
    const showSchemaStitch = semverCheck(
      'schemaStitching',
      this.props.serverVersion
    );
    if (showSchemaStitch) {
      this.setState({ ...this.state, showSchemaStitch: true });
    }
    return Promise.resolve();
  }
  checkEventsTab() {
    const showEvents = semverCheck('eventsTab', this.props.serverVersion);
    if (showEvents) {
      this.setState({ showEvents: true });
    }
    return Promise.resolve();
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
  closeLoveIcon() {
    const s = {
      isDismissed: true,
    };
    setLoveConsentState(s);
    this.setState({
      ...this.state,
      loveConsentState: { ...getLoveConsentState() },
    });
  }
  closeUpdateBanner() {
    const { latestServerVersion } = this.props;
    window.localStorage.setItem(
      latestServerVersion + '_BANNER_NOTIFICATION_CLOSED',
      'true'
    );
    this.setState({ showBannerNotification: false });
  }

  render() {
    const {
      children,
      location,
      migrationModeProgress,
      currentSchema,
      serverVersion,
      latestServerVersion,
    } = this.props;
    const styles = require('./Main.scss');
    const appPrefix = '';
    const logo = require('./white-logo.svg');
    const github = require('./Github.svg');
    const discord = require('./Discord.svg');
    const mail = require('./mail.svg');
    const docs = require('./logo.svg');
    const pixHeart = require('./pix-heart.svg');
    const currentLocation = location.pathname;
    const currentActiveBlock = currentLocation.split('/')[1];

    const sidebarClass = styles.sidebar;

    let mainContent = null;
    if (migrationModeProgress) {
      mainContent = (
        <div>
          {' '}
          <Spinner />{' '}
        </div>
      );
    } else {
      mainContent = children && React.cloneElement(children);
    }
    let accessKeyHtml = null;
    if (
      !globals.isAccessKeySet &&
      (globals.accessKey === '' || globals.accessKey === null)
    ) {
      accessKeyHtml = (
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

    return (
      <div className={styles.container}>
        <div className={styles.flexRow}>
          <div className={sidebarClass}>
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
                {this.state.showSchemaStitch ? (
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
                        to={appPrefix + '/remote-schemas'}
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
                ) : null}
                {this.state.showEvents ? (
                  <OverlayTrigger placement="right" overlay={tooltip.events}>
                    <li>
                      <Link
                        className={
                          currentActiveBlock === 'events'
                            ? styles.navSideBarActive
                            : ''
                        }
                        to={appPrefix + '/events'}
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
                ) : null}
              </ul>
            </div>
            <div id="dropdown_wrapper" className={styles.clusterInfoWrapper}>
              {accessKeyHtml}
              <Link to="/metadata">
                <div className={styles.helpSection + ' ' + styles.settingsIcon}>
                  <i className={styles.question + ' fa fa-cog'} />
                  {currentActiveBlock === 'metadata' ? (
                    <span className={styles.selected} />
                  ) : null}
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
                    (this.state.loveConsentState.isDismissed
                      ? styles.help_dropdown_menu_heart_consented
                      : '')
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
              {!this.state.loveConsentState.isDismissed
                ? [
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
                            Star us on Github,
                          <br />
                            To make our <i className={'fa fa-heart'} /> go
                            wooooo!
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
                                src={
                                  'https://storage.googleapis.com/hasura-graphql-engine/console/assets/githubicon.png'
                                }
                                alt={'Github'}
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
                                src={
                                  'https://storage.googleapis.com/hasura-graphql-engine/console/assets/twittericon.png'
                                }
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
                ]
                : null}
            </div>
          </div>
          <div className={styles.main + ' container-fluid'}>{mainContent}</div>
          {this.state.showBannerNotification ? (
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
          ) : null}
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
  };
};

export default connect(mapStateToProps)(Main);
