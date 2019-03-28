import React from 'react';
import PropTypes from 'prop-types';

import { Link } from 'react-router';
import Helmet from 'react-helmet';

class ErrorBoundary extends React.Component {
  constructor(props) {
    super(props);
    this.state = { hasError: false, info: null, error: null };
  }

  componentDidCatch(error, info) {
    this.setState({ hasError: true, info: info, error: error });
    // TODO logErrorToMyService(error, info);
  }

  render() {
    const lostImage = require('./500-logo.png');
    const styles = require('./ErrorPage.scss');
    const cc = this.state.info ? this.state.info.componentStack.split(' in ') : [];
    const carray = [];
    let i;
    if (cc) {
      for (i = 0; i < cc.length; i++) {
        const linestyle = { marginLeft: 8 * i };
        const acomponent = cc[i].trim();
        if (acomponent.length > 0) {
          carray.push(
            <li className="component" style={linestyle}>{acomponent}</li>
          );
        }
      }
    }
    if (this.state.hasError) {
      return (
        <div className={styles.viewcontainer}>
          <Helmet title="500 - System Error | Hasura" />
          <div className={'container ' + styles.centerContent}>
            <div className={'row ' + styles.message}>
              <div className="col-xs-8">
                <h1>Oh Fudge</h1>
                            Something broke. Head back <Link to="/">Home</Link>.
              </div>
              <div className="col-xs-4">
                <img
                  src={lostImage}
                  className="img-responsive"
                  name="hasura"
                  title="Sorry, we broke that!"
                />
              </div>
              <div className="col-xs-12">
                {this.state.error && this.state.error.toString()}
              </div>
              <div className="col-xs-12">
                <h2>Component Stack:</h2>
                <ul>
                  {carray}
                </ul>
              </div>
            </div>
          </div>
        </div>
      );
    }
    return this.props.children;
  }
}

ErrorBoundary.propTypes = {
  children: PropTypes.element,
};

export default ErrorBoundary;
