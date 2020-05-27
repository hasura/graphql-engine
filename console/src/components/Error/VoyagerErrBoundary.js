import React, { Component } from 'react';
import PropTypes from 'prop-types';
import globals from '../../Globals';

class VoyagerErrBoundary extends Component {
  constructor(props) {
    super(props);
    this.state = {
      error: null,
      errorInfo: null,
    };
  }

  componentDidCatch(error, errorInfo) {
    this.setState({
      errorInfo,
      error,
    });

    // TODO: log error in redux? other service?
  }

  render() {
    const errorImage = `${globals.assetsPath}/common/img/hasura_icon_green.svg`;
    const styles = require('./ErrorPage.scss');

    if (this.state.errorInfo) {
      return (
        <div className={styles.viewContainer}>
          <div className={`container ${styles.centerContent}`}>
            <div className={`row ${styles.message}`}>
              <div className="col-xs-8">
                <h1>Error on VoyagerView!</h1>
                <br />
                <div>
                  <pre className={styles.errorStack}>
                    {this.state.error && this.state.error.toString()}
                    {this.state.errorInfo.componentStack}
                  </pre>
                </div>
                <br />
              </div>
              <div className="col-xs-4">
                <img
                  src={errorImage}
                  className="img-responsive"
                  title="Something went wrong!"
                  alt="errored in voyager view"
                />
              </div>
            </div>
          </div>
        </div>
      );
    }

    return this.props.children;
  }
}

VoyagerErrBoundary.propTypes = {
  children: PropTypes.node,
};

export default VoyagerErrBoundary;
