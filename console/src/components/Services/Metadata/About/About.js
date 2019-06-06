import React, { Component } from 'react';
import styles from './About.scss';

import Endpoints from '../../../../Endpoints';
import Star from '../../../Common/Icons/Star';

class About extends Component {
  state = {
    serverVersion: '',
    serverVersionLoading: true,
    latestVersionAvailable: '',
    latestVersionAvailableLoading: true,
  };

  componentDidMount() {
    fetch(Endpoints.version)
      .then(response => response.json())
      .then(serverVersion =>
        this.setState({
          serverVersion: serverVersion.version,
          serverVersionLoading: false,
        })
      );

    fetch(Endpoints.updateCheck)
      .then(response => response.json())
      .then(latest =>
        this.setState({
          latestVersionAvailable: latest.latest,
          latestVersionAvailableLoading: false,
        })
      );
  }

  render() {
    if (
      this.state.serverVersionLoading &&
      this.state.latestVersionAvailableLoading
    ) {
      return (
        <div
          className={`${styles.clear_fix} ${styles.padd_left} ${
            styles.padd_top
          } ${styles.metadata_wrapper} container-fluid`}
        >
          <div className={styles.subHeader}>
            <h2
              className={`${styles.heading_text} ${styles.remove_pad_bottom}`}
            >
              Server Version
            </h2>
            <div className={styles.add_mar_top + ' ' + styles.wd60}>
              <Star /> <em>...loading</em>
            </div>
            <hr />
            <h2
              className={`${styles.heading_text} ${styles.remove_pad_bottom}`}
            >
              Latest Server Version Available
            </h2>
            <div className={styles.add_mar_top + ' ' + styles.wd60}>
              <Star /> <em>...loading</em>
            </div>
            <hr />
          </div>
        </div>
      );
    }
    return (
      <div
        className={`${styles.clear_fix} ${styles.padd_left} ${
          styles.padd_top
        } ${styles.metadata_wrapper} container-fluid`}
      >
        <div className={styles.subHeader}>
          <h2
            className={`${styles.heading_text} ${styles.remove_pad_bottom}`}
          >
              Server Version
          </h2>
          <div className={styles.add_mar_top + ' ' + styles.wd60}>
            <Star /> {this.state.serverVersion}
          </div>
          <hr />
          <h2
            className={`${styles.heading_text} ${styles.remove_pad_bottom}`}
          >
              Latest Server Version Available
          </h2>
          <div className={styles.add_mar_top + ' ' + styles.wd60}>
            <Star /> {this.state.latestVersionAvailable}
          </div>
          <hr />
        </div>
      </div>
    );
  }
}

const mapStateToProps = state => {
  return {
    about: state.metadata.about,
  };
};

const aboutConnector = connect => connect(mapStateToProps)(About);

export default aboutConnector;
