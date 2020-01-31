import React, { Component } from 'react';
import GraphiQL from 'graphiql';
import PropTypes from 'prop-types';
import GraphiQLErrorBoundary from './GraphiQLErrorBoundary';
import OneGraphExplorer from '../OneGraphExplorer/OneGraphExplorer';
import AnalyzeButton from '../Analyzer/AnalyzeButton';

import {
  clearCodeMirrorHints,
  setQueryVariableSectionHeight,
  copyToClipboard,
} from './utils';
import { analyzeFetcher, graphQLFetcherFinal } from '../Actions';
import { parse, print } from 'graphql';

import 'graphiql/graphiql.css';
import './GraphiQL.css';

class GraphiQLWrapper extends Component {
  constructor(props) {
    super(props);
    this.state = {
      schema: null,
      error: false,
      noSchema: false,
      onBoardingEnabled: false,
      copyButtonText: 'Copy',
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

    let graphiqlContext;

    const handleClickPrettifyButton = () => {
      const editor = graphiqlContext.getQueryEditor();
      const currentText = editor.getValue();
      const prettyText = print(parse(currentText));
      editor.setValue(prettyText);
    };

    const handleCopyQuery = () => {
      const editor = graphiqlContext.getQueryEditor();
      const query = editor.getValue();

      if (!query) {
        return;
      }
      copyToClipboard(query);
      this.setState({ copyButtonText: 'Copied' });
      setTimeout(() => {
        this.setState({ copyButtonText: 'Copy' });
      }, 1500);
    };

    const handleToggleHistory = () => {
      graphiqlContext.setState(prevState => ({
        historyPaneOpen: !prevState.historyPaneOpen,
      }));
    };

    const renderGraphiql = graphiqlProps => {
      const voyagerUrl = graphqlNetworkData.consoleUrl + '/voyager-view';
      let analyzerProps = {};
      if (graphiqlContext) {
        analyzerProps = graphiqlContext.state;
      }

      // get toolbar buttons
      const getGraphiqlButtons = () => {
        const buttons = [];
        buttons.push({
          label: 'Prettify',
          title: 'Prettify Query (Shift-Ctrl-P)',
          onClick: handleClickPrettifyButton,
        });
        buttons.push({
          label: 'History',
          title: 'Show History',
          onClick: handleToggleHistory,
        });
        buttons.push({
          label: this.state.copyButtonText,
          title: 'Copy Query',
          onClick: handleCopyQuery,
        });
        buttons.push({
          label: 'Explorer',
          title: 'Toggle Explorer',
          onClick: graphiqlProps.toggleExplorer,
        });
        buttons.push({
          label: 'Voyager',
          title: 'GraphQL Voyager',
          onClick: () => window.open(voyagerUrl, '_blank'),
          icon: <i className="fa fa-external-link" aria-hidden="true" />,
        });
        return buttons.map(b => {
          return <GraphiQL.Button key={b.label} {...b} />;
        });
      };

      return (
        <GraphiQL
          ref={c => {
            graphiqlContext = c;
          }}
          fetcher={graphQLFetcher}
          voyagerUrl={voyagerUrl}
          analyzeFetcher={analyzeFetcherInstance}
          {...graphiqlProps}
        >
          <GraphiQL.Logo>GraphiQL</GraphiQL.Logo>
          <GraphiQL.Toolbar>
            {getGraphiqlButtons()}
            <AnalyzeButton
              operations={graphiqlContext && graphiqlContext.state.operations}
              analyzeFetcher={analyzeFetcherInstance}
              {...analyzerProps}
            />
          </GraphiQL.Toolbar>
        </GraphiQL>
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
            headersInitialised={graphqlNetworkData.headersInitialised}
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
