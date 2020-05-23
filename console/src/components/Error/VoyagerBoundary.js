import React, { Component } from 'react';
import PropTypes from 'prop-types';
import globals from '../../Globals';

// #4818 Issue

class VoyagerBoundary extends Component {
  constructor() {
    super();
    this.state = {
      error: false,
      errorInfo: null,
    };
  }

  componentDidCatch(error, errorInfo) {
    this.setState({
      error,
      errorInfo,
    });
  }

  render() {
    const errorImage = `${globals.assetsPath}/common/img/hasura_icon_green.svg`;
    const styles = require('./ErrorPage.scss');

    if (this.state.errorInfo) {
      return (
        <div className={styles.viewContainer}>
          <div className={'container ' + styles.centerContent}>
            <div className={'row ' + styles.message}>
              <div className="col-xs-8">
                <h1>Error</h1>
                <br />
                <div>
                  Something went wrong.
                  <details>
                    <summary>Error Summary</summary>
                    {this.state.error && this.state.error.toString()}
                    <br />
                    <div>
                      <pre className={styles.errorStack}>
                        {this.state.errorInfo.componentStack}
                      </pre>
                    </div>
                  </details>
                </div>
                <br />
              </div>
              <div className="col-xs-4">
                <img
                  src={errorImage}
                  className="img-responsive"
                  name="hasura"
                  title="Something went wrong!"
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

VoyagerBoundary.propTypes = {
  dispatch: PropTypes.func.isRequired,
};

export default VoyagerBoundary;
