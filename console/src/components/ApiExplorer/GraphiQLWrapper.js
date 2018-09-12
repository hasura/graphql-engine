import React, { Component } from 'react';
import GraphiQL from 'hasura-console-graphiql';
import PropTypes from 'prop-types';
import ErrorBoundary from './ErrorBoundary';
import { graphQLFetcherFinal, getRemoteQueries } from './Actions';

import './GraphiQL.css';

class GraphiQLWrapper extends Component {
  constructor(props) {
    super(props);
    this.state = {
      schema: null,
      error: false,
      noSchema: false,
      onBoardingEnabled: false,
      queries: null,
    };
    const queryFile = this.props.queryParams
      ? this.props.queryParams.query_file
      : null;
    if (queryFile) {
      getRemoteQueries(queryFile, queries =>
        this.setState({ ...this.state, queries })
      );
    }
  }

  shouldComponentUpdate(nextProps) {
    return !nextProps.headerFocus;
  }

  render() {
    const styles = require('../Common/Common.scss');
    const graphQLFetcher = graphQLParams => {
      if (this.state.headerFocus) {
        return null;
      }
      return graphQLFetcherFinal(
        graphQLParams,
        this.props.data.url,
        this.props.data.headers
      );
    };

    // let content = "fetching schema";
    let content = (
      <i className={'fa fa-spinner fa-spin ' + styles.graphSpinner} />
    );

    if (!this.state.error && this.props.numberOfTables !== 0) {
      if (this.state.queries) {
        content = (
          <GraphiQL fetcher={graphQLFetcher} query={this.state.queries} />
        );
      } else {
        content = <GraphiQL fetcher={graphQLFetcher} />;
      }
    } else if (this.props.numberOfTables === 0) {
      content = (
        <GraphiQL
          fetcher={graphQLFetcher}
          query={
            '# Looks like you do not have any tables.\n# Click on the "Data" tab on top to create tables\n# You can come back here and try out the GraphQL queries after you create tables\n'
          }
          schema={undefined}
        />
      );
    } else if (this.state.error) {
      // there is an error parsing graphql schema
      content = <div> Error parsing GraphQL Schema </div>;
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
          {content}
        </div>
      </ErrorBoundary>
    );
  }
}

GraphiQLWrapper.propTypes = {
  dispatch: PropTypes.func.isRequired,
  data: PropTypes.object.isRequired,
  numberOfTables: PropTypes.number.isRequired,
  headerFocus: PropTypes.bool.isRequired,
};

export default GraphiQLWrapper;
