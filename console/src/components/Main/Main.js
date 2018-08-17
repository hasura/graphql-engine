import React from 'react';
import { connect } from 'react-redux';
import { Link } from 'react-router';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import globals from '../../Globals';
import * as tooltip from './Tooltips';
import 'react-toggle/style.css';
import Spinner from '../Common/Spinner/Spinner';
import { loadServerVersion, checkServerUpdates } from './Actions';

const semver = require('semver');

class Main extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      showBannerNotification: false,
    };
  }
  componentDidMount() {
    const { dispatch } = this.props;
    dispatch(loadServerVersion()).then(() => {
      dispatch(checkServerUpdates()).then(() => {
        let isUpdateAvailable = false;
        try {
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
    const logo = require('./logo.svg');
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
    if (globals.accessKey === '' || globals.accessKey === null) {
      accessKeyHtml = (
        <OverlayTrigger placement="left" overlay={tooltip.secureEndpoint}>
          <a href="https://docs.hasura.io/1.0/graphql/manual/deployment/securing-graphql-endpoint.html">
            <button className={'btn btn-danger ' + styles.add_mar_right}>
              <i
                className={
                  styles.padd_small_right + ' fa fa-exclamation-triangle'
                }
              />
              Secure your endpoint
            </button>
          </a>
        </OverlayTrigger>
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
                  <div className={styles.header_project_name}>HASURA</div>
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
              </ul>
            </div>
            <div className={styles.clusterInfoWrapper}>
              {accessKeyHtml}
              <div className={styles.spreadWord}>
                <button
                  className={styles.yellow_button + ' dropdown-toggle'}
                  data-toggle="dropdown"
                  aria-haspopup="true"
                  aria-expanded="false"
                >
                  Share the <i className={styles.heart + ' fa fa-heart'} />
                </button>
                <ul className={'dropdown-menu ' + styles.dropdown_menu}>
                  <div className={styles.arrow_up_dropdown} />
                  <li className={'dropdown-item '}>
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
                  </li>
                  <li className={'dropdown-item '}>
                    <div className="btn-o">
                      <a
                        href="https://twitter.com/intent/tweet?hashtags=demo&amp;text=Hello%20world;url=https%3A%2F%2Fgithub.com%2Fgraphql-engine"
                        className={styles.twitterShare}
                      >
                        <span className="label">Tweet</span>
                      </a>
                    </div>
                  </li>
                </ul>
                <i className={styles.question + ' fa fa-question-circle'} />
              </div>
            </div>
          </div>
          <div className={styles.main + ' container-fluid'}>{mainContent}</div>
          {this.state.showBannerNotification ? (
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
