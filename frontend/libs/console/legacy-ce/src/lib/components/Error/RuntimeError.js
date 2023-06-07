import PropTypes from 'prop-types';
import React, { Component } from 'react';
import { connect } from 'react-redux';

import { Link } from 'react-router';
import Helmet from 'react-helmet';
import globals from '../../Globals';

class RuntimeError extends Component {
  render() {
    const errorImage = `${globals.assetsPath}/common/img/hasura_icon_green.svg`;

    const { resetCallback, error } = this.props;
    return (
      <div className="h-screen w-screen flex items-center justify-center">
        <Helmet title="Error | Hasura" />

        <div className="flex-row flex justify-center">
          <div className="w-6/12">
            <h1 className="font-bold text-6xl">Error</h1>
            <br />
            <div>
              Something went wrong. Head back{' '}
              <Link to="/" onClick={resetCallback}>
                Home
              </Link>
              .
            </div>
            <br />
            <div>
              <pre className={''}>{error.stack}</pre>
            </div>
            <br />
            <div>
              You can report this issue on our{' '}
              <a href="https://github.com/hasura/graphql-engine/issues">
                GitHub
              </a>
            </div>
          </div>
          <div className="w-1/6 pl-16">
            <img
              src={errorImage}
              name="hasura"
              title="Something went wrong!"
              alt="something went wrong"
            />
          </div>
        </div>
      </div>
    );
  }
}

RuntimeError.propTypes = {
  dispatch: PropTypes.func.isRequired,
  resetCallback: PropTypes.func.isRequired,
};

export default connect()(RuntimeError);
