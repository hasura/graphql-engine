import Helmet from 'react-helmet';
import React, { Component } from 'react';

import globals from '../../../Globals';
import styles from '../../Error/ErrorPage.scss';

const errorImageSrc = `${globals.assetsPath}/common/img/hasura_icon_green.svg`;

interface ErrorState {
  error?: Error | null;
}

type Props = {
  title?: string;
  message: string;
  helmetTitle: string;
};

class ErrorBoundary extends Component<Props, ErrorState> {
  state: ErrorState = {
    error: null,
  };

  componentDidCatch(error: Error) {
    this.setState({
      error,
    });
  }

  render() {
    if (this.state.error) {
      return (
        <div className={styles.viewContainer}>
          <Helmet title={this.props.helmetTitle} />
          <div className={`container ${styles.centerContent}`}>
            <div className={`row ${styles.message}`}>
              <div className="col-xs-8">
                <h1>{this.props.title || 'Error'}</h1>
                <br />
                <h4 className={styles.errorDescription}>
                  {this.props.message}
                </h4>
                <div>
                  <pre className={styles.errorStack}>
                    {this.state.error.stack}
                  </pre>
                </div>
                <br />
              </div>
              <div className="col-xs-4">
                <img
                  src={errorImageSrc}
                  className="img-responsive"
                  title={this.props.title}
                  alt="hasura-logo"
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

export default ErrorBoundary;
