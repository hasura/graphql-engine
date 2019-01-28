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

    if (!this.state.error && this.props.numberOfTables !== 0) {
      if (this.state.queries) {
        content = (
          <GraphiQL
            fetcher={graphQLFetcher}
            analyzeFetcher={analyzeFetcherInstance}
            supportAnalyze={supportAnalyze}
            query={this.state.queries}
          />
        );
      } else {
        content = (
          <GraphiQL
            fetcher={graphQLFetcher}
            analyzeFetcher={analyzeFetcherInstance}
            supportAnalyze={supportAnalyze}
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
