import React, { Component, ReactNode } from 'react';
import globals from '../../Globals';

type State = {
  error: object | null;
  // FIXME: remove any
  // fix type for 'componentStack'
  errorInfo: any;
  hasError: boolean;
};

type Props = {
  children: ReactNode
};

class VoyagerErrBoundary extends Component<Props, State> {
  readonly state : State = {
    hasError: false,
    error: null,
    errorInfo: null,
  }

  componentDidCatch(error: object, errorInfo: object) {
    this.setState({
      hasError: true,
      error,
      errorInfo,
    });
  }

  render() {
    const errorImage = `${globals.assetsPath}/common/img/hasura_icon_green.svg`;
    const styles = require('./ErrorPage.scss');

    if (this.state.hasError) {
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

export default VoyagerErrBoundary;
