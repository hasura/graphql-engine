import React, { Component } from 'react';
import GraphiQL from 'hasura-console-graphiql';
import { getHeadersAsJSON } from './utils';
import { getIntrospectionQuery, buildClientSchema } from 'graphql';
import PropTypes from 'prop-types';
import ErrorBoundary from './ErrorBoundary';
import { graphQLFetcherFinal, getRemoteQueries } from './Actions';
import GraphiQLExplorer from 'graphiql-explorer';
import { makeDefaultArg, getDefaultScalarArgValue } from './onegraphUtils';

import './GraphiQL.css';

class GraphiQLWrapper extends Component {

  constructor(props) {
    super(props);
    this.state = {
      schema: null,
      error: false,
      queries: null,
      onBoardingEnabled: false,
      explorerIsOpen: true,
      query: ''
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

  _handleEditQuery = (query: string): void => this.setState({query});

  _handleToggleExplorer = () => {
    this.setState({explorerIsOpen: !this.state.explorerIsOpen});
  }

  shouldComponentUpdate(nextProps) {
    return !nextProps.headerFocus;
  }

  introspect() {
    const { url, headers } = this.props.data;
    fetch(url, {
      method: 'POST',
      headers: getHeadersAsJSON(headers || []),
      body: JSON.stringify({
        query: getIntrospectionQuery(),
      }),
    })
    .then(response => response.json())
    .then(result => {
      this.setState({
        schema: buildClientSchema(result.data)
      });
    })
    .catch(error => {
      this.setState({
        schema: null
      });
    })
  }

  componentDidMount() {
    this.introspect();
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
    } else if (this.state.queries) {
      graphiqlProps.query = this.state.queries;
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
          <div className="graphiql-container">
           <GraphiQLExplorer
              schema={this.state.schema}
              query={this.state.query}
              onEdit={this._handleEditQuery}
              explorerIsOpen={this.state.explorerIsOpen}
              onToggleExplorer={this._handleToggleExplorer}
              getDefaultScalarArgValue={getDefaultScalarArgValue}
              makeDefaultArg={makeDefaultArg}
            />
            <GraphiQL {...graphiqlProps}
              ref={ref => (this._graphiql = ref)}
              schema={this.state.schema}
              query={this.state.query}
              onEditQuery={this._handleEditQuery}
            >
              <GraphiQL.Toolbar>
                <GraphiQL.Button
                  onClick={() => this._graphiql.handlePrettifyQuery()}
                  label="Prettify"
                  title="Prettify Query (Shift-Ctrl-P)"
                />
                <GraphiQL.Button
                  onClick={() => this._graphiql.handleToggleHistory()}
                  label="History"
                  title="Show History"
                />
                <GraphiQL.Button
                  onClick={this._handleToggleExplorer}
                  label="Explorer"
                  title="Toggle Explorer"
                />
              </GraphiQL.Toolbar>
            </GraphiQL>
          </div>
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
