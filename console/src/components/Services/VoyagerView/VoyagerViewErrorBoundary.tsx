import React, { Component, ReactNode } from 'react';
import globals from '../../../Globals';
import styles from '../../Error/ErrorPage.scss';

const errorImageSrc = `${globals.assetsPath}/common/img/hasura_icon_green.svg`;

interface ErrorState {
  error: Error;
  hasError: boolean;
}

type Props = {
  children: ReactNode;
};

class VoyagerViewErrorBoundary extends Component<Props, ErrorState> {
  state: ErrorState = {
    hasError: false,
    error: new Error(''),
  };

  componentDidCatch(error: Error) {
    this.setState({
      hasError: true,
      error,
    });
  }

  render() {
    if (this.state.hasError) {
      return (
        <div className={styles.viewContainer}>
          <div className={`container ${styles.centerContent}`}>
            <div className={`row ${styles.message}`}>
              <div className="col-xs-8">
                <h1>Error in Voyager View</h1>
                <br />
                <h3>
                  You might be seeing this because your schema couldn&apos;t
                  rendered.
                </h3>
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
                  title="Something went wrong!"
                  alt=""
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

export default VoyagerViewErrorBoundary;
