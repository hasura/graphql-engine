import React, { Component } from 'react';
import GraphiQL from 'hasura-console-graphiql';
import PropTypes from 'prop-types';
import ErrorBoundary from './ErrorBoundary';
import {
  analyzeFetcher,
  graphQLFetcherFinal,
  getRemoteQueries,
} from './Actions';
import OneGraphExplorer from './OneGraphExplorer';
import './GraphiQL.css';
import semverCheck from '../../helpers/semver';

const NO_TABLES_MESSAGE =
  '# Looks like you do not have any tables.\n# Click on the "Data" tab on top to create tables\n# You can come back here and try out the GraphQL queries after you create tables\n';

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
      getRemoteQueries(queryFile, queries => this.setState({ queries }));
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
      supportAnalyze: supportAnalyze,
    });
  }
  updateAnalyzeApiState(analyzeApiChange) {
    this.setState({
      analyzeApiChange: analyzeApiChange,
    });
  }

  render() {
    const styles = require('../Common/Common.scss');
    const {
      supportAnalyze,
      analyzeApiChange,
      queries,
      headerFocus,
    } = this.state;
    const { numberOfTables } = this.props;
    const graphqlNetworkData = this.props.data;
    const graphQLFetcher = graphQLParams => {
      if (headerFocus) {
        return null;
      }
      return graphQLFetcherFinal(
        graphQLParams,
        graphqlNetworkData.url,
        graphqlNetworkData.headers
      );
    };
    const analyzeFetcherInstance = analyzeFetcher(
      graphqlNetworkData.url,
      graphqlNetworkData.headers,
      analyzeApiChange
    );
    const queryString = numberOfTables ? queries : NO_TABLES_MESSAGE;
    const renderGraphiql = graphiqlProps => {
      return (
        <GraphiQL
          fetcher={graphQLFetcher}
          analyzeFetcher={analyzeFetcherInstance}
          supportAnalyze={supportAnalyze}
          query={queryString}
          {...graphiqlProps}
        />
      );
    };
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
          <OneGraphExplorer
            renderGraphiql={renderGraphiql}
            endpoint={graphqlNetworkData.url}
            headers={graphqlNetworkData.headers}
            headerFocus={headerFocus}
            query={queryString}
          />
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
