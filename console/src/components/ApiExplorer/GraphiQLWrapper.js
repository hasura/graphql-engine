import React, { Component } from 'react';
import GraphiQL from 'hasura-console-graphiql';
import PropTypes from 'prop-types';
import ErrorBoundary from './ErrorBoundary';
import {
  analyzeFetcher,
  graphQLFetcherFinal,
  getRemoteQueries,
} from './Actions';

import './GraphiQL.css';

import semverCheck from '../../helpers/semver';

class GraphiQLWrapper extends Component {
  constructor(props) {
    super(props);
    this.state = {
      schema: null,
      error: false,
      noSchema: false,
      onBoardingEnabled: false,
      queries: null,
      supportAnalyze: false,
      analyzeApiChange: false,
      variables: null,
      onEditQuery: null,
      onEditVariables: null,
    };
    const queryFile = this.props.queryParams
      ? this.props.queryParams.query_file
      : null;
    const queryString = this.props.queryParams
      ? this.props.queryParams.query_string
      : null;
    const queryVariables = this.props.queryParams
      ? this.props.queryParams.query_variables
      : null;
    if (queryFile) {
      getRemoteQueries(queryFile, queries =>
        this.setState({ ...this.state, queries })
      );
    }

    if (queryString) {
      this.state.queries = queryString;
    }

    if (queryVariables) {
      this.state.variables = queryVariables;
    }

    this.onEditQuery = this.onEditQuery.bind(this);
    this.onEditVariables = this.onEditVariables.bind(this);
    this.updateURL = this.updateURL.bind(this);
  }

  componentDidMount() {
    if (this.props.data.serverVersion) {
      this.checkSemVer(this.props.data.serverVersion).then(() =>
        this.checkNewAnalyzeVersion(this.props.data.serverVersion)
      );
    }
  }

  componentWillReceiveProps(nextProps) {
    if (nextProps.data.serverVersion !== this.props.data.serverVersion) {
      this.checkSemVer(nextProps.data.serverVersion).then(() =>
        this.checkNewAnalyzeVersion(nextProps.data.serverVersion)
      );
    }
  }
  shouldComponentUpdate(nextProps) {
    return !nextProps.headerFocus;
  }

  checkSemVer(version) {
    try {
      const showAnalyze = semverCheck('sqlAnalyze', version);
      if (showAnalyze) {
        this.updateAnalyzeState(true);
      } else {
        this.updateAnalyzeState(false);
      }
    } catch (e) {
      this.updateAnalyzeState(false);
      console.error(e);
    }
    return Promise.resolve();
  }
  checkNewAnalyzeVersion(version) {
    try {
      const analyzeApiChange = semverCheck('analyzeApiChange', version);
      if (analyzeApiChange) {
        this.updateAnalyzeApiState(true);
      } else {
        this.updateAnalyzeApiState(false);
      }
    } catch (e) {
      this.updateAnalyzeApiState(false);
      console.error(e);
    }
    return Promise.resolve();
  }
  updateAnalyzeState(supportAnalyze) {
    this.setState({
      ...this.state,
      supportAnalyze: supportAnalyze,
    });
  }
  updateAnalyzeApiState(analyzeApiChange) {
    this.setState({
      ...this.state,
      analyzeApiChange: analyzeApiChange,
    });
  }

  onEditQuery(newQuery) {
    console.log(newQuery);
    this.setState({ ...this.state, onEditQuery: newQuery });
    this.updateURL();
  }

  onEditVariables(newVariables) {
    console.log(newVariables);
    this.setState({ ...this.state, onEditVariables: newVariables });
    this.updateURL();
  }

  updateURL() {
      var search = window.location.search;
      var parameters = {};

      search.substr(1).split('&').forEach(function (entry) {
        var eq = entry.indexOf('=');
        if (eq >= 0) {
          parameters[decodeURIComponent(entry.slice(0, eq))] =
            decodeURIComponent(entry.slice(eq + 1));
        }
      });
      
      parameters.query_string = this.state.onEditQuery;
      parameters.query_variables = JSON.stringify(JSON.parse(this.state.onEditVariables), null, 2);

      var newSearch = '?' + Object.keys(parameters).filter(function (key) {
        return Boolean(parameters[key]);
      }).map(function (key) {
        return encodeURIComponent(key) + '=' +
          encodeURIComponent(parameters[key]);
      }).join('&');
      history.replaceState(null, null, newSearch);
  }

  render() {
    const styles = require('../Common/Common.scss');
    const { supportAnalyze, analyzeApiChange } = this.state;
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

    const analyzeFetcherInstance = analyzeFetcher(
      this.props.data.url,
      this.props.data.headers,
      analyzeApiChange
    );

    // let content = "fetching schema";
    let content = (
      <i className={'fa fa-spinner fa-spin ' + styles.graphSpinner} />
    );

    if(!this.state.error && this.props.numberOfTables == 0 && this.state.queries) {
      content = (
        <GraphiQL
          fetcher={graphQLFetcher}
          analyzeFetcher={analyzeFetcherInstance}
          supportAnalyze={supportAnalyze}
          query={this.state.queries}
          variables={this.state.variables}
          onEditQuery={this.onEditQuery}
          onEditVariables={this.onEditVariables}
        />
      );
    }

    if (!this.state.error && this.props.numberOfTables !== 0) {
      if (this.state.queries) {
        content = (
          <GraphiQL
            fetcher={graphQLFetcher}
            analyzeFetcher={analyzeFetcherInstance}
            supportAnalyze={supportAnalyze}
            query={this.state.queries}
            variables={this.state.variables}
            onEditQuery={this.onEditQuery}
            onEditVariables={this.onEditVariables}
          />
        );
      }
      else {
        content = (
          <GraphiQL
            fetcher={graphQLFetcher}
            analyzeFetcher={analyzeFetcherInstance}
            supportAnalyze={supportAnalyze}
            onEditQuery={this.onEditQuery}
            onEditVariables={this.onEditVariables}
          />
        );
      }
    } else if (this.props.numberOfTables === 0) {
      content = (
        <GraphiQL
          fetcher={graphQLFetcher}
          supportAnalyze={supportAnalyze}
          analyzeFetcher={analyzeFetcherInstance}
          query={
            '# Looks like you do not have any tables.\n# Click on the "Data" tab on top to create tables\n# You can come back here and try out the GraphQL queries after you create tables\n'
          }
          schema={undefined}
          onEditQuery={this.onEditQuery}
          onEditVariables={this.onEditVariables}
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
