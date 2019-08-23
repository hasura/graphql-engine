import React, { Component } from 'react';
import GraphiQL from 'hasura-console-graphiql';
import PropTypes from 'prop-types';
import GraphiQLErrorBoundary from './GraphiQLErrorBoundary';
import OneGraphExplorer from '../OneGraphExplorer/OneGraphExplorer';

import { clearCodeMirrorHints, setQueryVariableSectionHeight } from './utils';
import { analyzeFetcher, graphQLFetcherFinal } from '../Actions';

import './GraphiQL.css';

class GraphiQLWrapper extends Component {
  constructor(props) {
    super(props);
    this.state = {
      schema: null,
      error: false,
      noSchema: false,
      onBoardingEnabled: false,
    };
  }

  componentDidMount() {
    setQueryVariableSectionHeight();
  }

  componentWillUnmount() {
    clearCodeMirrorHints();
  }

  render() {
    const styles = require('../../../Common/Common.scss');

    const { numberOfTables, urlParams, headerFocus } = this.props;
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
      graphqlNetworkData.headers
    );

    const renderGraphiql = graphiqlProps => {
      const voyagerUrl = graphqlNetworkData.consoleUrl + '/voyager-view';
      return (
        <GraphiQL
          fetcher={graphQLFetcher}
          supportAnalyze
          voyagerUrl={voyagerUrl}
          analyzeFetcher={analyzeFetcherInstance}
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
