import React, { Component, ReactNode } from 'react';
import globals from '../../Globals';
import styles from './ErrorPage.scss';

type State = {
  error: object | null;
  // FIXME: remove any
  // fix type for 'componentStack'
  errorInfo: any;
};

type Props = {
  children: ReactNode;
};

export default class VoyagerErrBoundary extends Component<Props, State> {
  readonly state: State = {
    error: null,
    errorInfo: null,
  };

  static getDerivedStateFromError(error: object) {
    return { error };
  }

  componentDidCatch(_: any, errorInfo: object) {
    this.setState({
      errorInfo,
    });
  }

  render() {
    const errorImage = `${globals.assetsPath}/common/img/hasura_icon_green.svg`;

    if (this.state.error) {
      return (
        <div className={styles.viewContainer}>
          <div className={`container ${styles.centerContent}`}>
            <div className={`row ${styles.message}`}>
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
