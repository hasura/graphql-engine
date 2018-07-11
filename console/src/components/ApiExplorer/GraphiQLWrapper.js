import React, { Component } from 'react';
import GraphiQL from 'graphiql';
import PropTypes from 'prop-types';
import ErrorBoundary from './ErrorBoundary';
import { graphQLFetcherFinal } from './Actions';

import './GraphiQL.css';

class GraphiQLWrapper extends Component {
  constructor() {
    super();
    this.state = {
      schema: null,
      error: false,
      noSchema: false,
      onBoardingEnabled: false,
    };
  }

  shouldComponentUpdate() {
    // shouldn't re-render if headers change. play button will trigger the query
    return false;
  }

  render() {
    const styles = require('../Common/Common.scss');

    const graphqlUrl = this.props.data.url;

    const graphQLFetcher = graphQLParams => {
      return graphQLFetcherFinal(
        graphQLParams,
        graphqlUrl,
        this.props.data.headers
      );
    };

    // let content = "fetching schema";
    let content = (
      <i className={'fa fa-spinner fa-spin ' + styles.graphSpinner} />
    );

    if (!this.state.error && !this.state.noSchema) {
      content = (
        <GraphiQL
          fetcher={graphQLFetcher}
          defaultQuery={''}
          schema={undefined}
        />
      );
    } else if (this.state.noSchema) {
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
};

export default GraphiQLWrapper;
