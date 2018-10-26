import React, { Component } from 'react';
import GraphiQL from 'hasura-console-graphiql';
import PropTypes from 'prop-types';
import ErrorBoundary from './ErrorBoundary';
import { graphQLFetcherFinal } from './Actions';

import './GraphiQL.css';

class GraphiQLWrapper extends Component {
  constructor(props) {
    super(props);
    this.state = {
      schema: null,
      error: false,
      onBoardingEnabled: false,
    };
  }

  shouldComponentUpdate(nextProps) {
    return !nextProps.headerFocus;
  }

  render() {
    const styles = require('../Common/Common.scss');
    const { variables, query } = window.__env;
    const graphQLFetcher = graphQLParams => {
      return graphQLFetcherFinal(
        graphQLParams,
        this.props.data.url,
        this.props.data.headers
      );
    };
    const graphiqlProps = {
      fetcher: graphQLFetcher,
    };
    if (query || Object.keys(variables).length !== 0) {
      graphiqlProps.query = query;
      if (variables !== 'undefined') {
        graphiqlProps.variables = JSON.stringify(variables, null, 2);
      }
    }
    return (
      <ErrorBoundary>
        <div
          className={
            'react-container-graphql ' +
            styles.wd100 +
            ' ' +
            styles.graphQLHeight
          }
        >
          <GraphiQL {...graphiqlProps} />
        </div>
      </ErrorBoundary>
    );
  }
}

GraphiQLWrapper.propTypes = {
  dispatch: PropTypes.func.isRequired,
  data: PropTypes.object.isRequired,
  headerFocus: PropTypes.bool.isRequired,
};

export default GraphiQLWrapper;
