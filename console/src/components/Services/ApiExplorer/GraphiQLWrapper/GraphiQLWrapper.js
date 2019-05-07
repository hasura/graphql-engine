import React, { Component } from 'react';
import GraphiQL from 'hasura-console-graphiql';
import PropTypes from 'prop-types';
import GraphiQLErrorBoundary from './GraphiQLErrorBoundary';
import OneGraphExplorer from '../OneGraphExplorer/OneGraphExplorer';

import { clearCodeMirrorHints, setQueryVariableSectionHeight } from './utils';
import { analyzeFetcher, graphQLFetcherFinal } from '../Actions';
import semverCheck from '../../../../helpers/semver';

import './GraphiQL.css';

class GraphiQLWrapper extends Component {
  constructor(props) {
    super(props);
    this.state = {
      schema: null,
      error: false,
      noSchema: false,
      onBoardingEnabled: false,
      supportAnalyze: false,
      analyzeApiChange: false,
    };
  }

  componentDidMount() {
    if (this.props.data.serverVersion) {
      this.checkSemVer(this.props.data.serverVersion).then(() =>
        this.checkNewAnalyzeVersion(this.props.data.serverVersion)
      );
    }

    setQueryVariableSectionHeight();
  }

  componentWillReceiveProps(nextProps) {
    if (nextProps.data.serverVersion !== this.props.data.serverVersion) {
      this.checkSemVer(nextProps.data.serverVersion).then(() =>
        this.checkNewAnalyzeVersion(nextProps.data.serverVersion)
      );
    }
  }

  componentWillUnmount() {
    clearCodeMirrorHints();
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
    const styles = require('../../../Common/Common.scss');

    const { supportAnalyze, analyzeApiChange, headerFocus } = this.state;

    const { numberOfTables, urlParams } = this.props;
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

    const renderGraphiql = graphiqlProps => {
      return (
        <GraphiQL
          fetcher={graphQLFetcher}
          analyzeFetcher={analyzeFetcherInstance}
          supportAnalyze={supportAnalyze}
          {...graphiqlProps}
        />
      );
    };

    return (
      <GraphiQLErrorBoundary>
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
            urlParams={urlParams}
            numberOfTables={numberOfTables}
          />
        </div>
      </GraphiQLErrorBoundary>
    );
  }
}

GraphiQLWrapper.propTypes = {
  dispatch: PropTypes.func.isRequired,
  data: PropTypes.object.isRequired,
  numberOfTables: PropTypes.number.isRequired,
  headerFocus: PropTypes.bool.isRequired,
  urlParams: PropTypes.object.isRequired,
};

export default GraphiQLWrapper;
