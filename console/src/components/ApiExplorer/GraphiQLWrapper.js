import React, { Component } from 'react';
import GraphiQL from 'graphiql';
import PropTypes from 'prop-types';
import fetch from 'isomorphic-fetch';
import { getHeadersAsJSON } from './utils';
import ErrorBoundary from './ErrorBoundary';

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

  render() {
    const styles = require('../Common/Common.scss');
    const headers = getHeadersAsJSON(this.props.data.headers);
    const graphqlUrl = this.props.data.url;
    const filterAccessKeyHeader = this.props.data.headers.filter(
      elem => elem.key === 'X-Hasura-Access-Key'
    )[0];

    if (
      this.props.data.dataHeaders['X-Hasura-Access-Key'] &&
      !filterAccessKeyHeader
    ) {
      headers['X-Hasura-Access-Key'] = this.props.data.dataHeaders[
        'X-Hasura-Access-Key'
      ];
    }
    const graphQLFetcher = graphQLParams => {
      return fetch(graphqlUrl, {
        method: 'POST',
        headers: headers,
        body: JSON.stringify(graphQLParams),
      }).then(response => response.json());
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
