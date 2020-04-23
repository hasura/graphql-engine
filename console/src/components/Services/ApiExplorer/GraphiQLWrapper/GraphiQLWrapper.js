import React, { Component } from 'react';
import { push } from 'react-router-redux';
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
import { parse as sdlParse, print } from 'graphql';
import deriveAction from '../../../../shared/utils/deriveAction';
import {
  getActionDefinitionSdl,
  getTypesSdl,
} from '../../../../shared/utils/sdlUtils';
import { showErrorNotification } from '../../Common/Notification';
import { getActionsCreateRoute } from '../../../Common/utils/routesUtils';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import {
  setActionDefinition,
  setTypeDefinition,
  setDerivedActionParentOperation,
} from '../../Actions/Add/reducer';

import 'graphiql/graphiql.css';
import './GraphiQL.css';

class GraphiQLWrapper extends Component {
  constructor(props) {
    super(props);
    this.state = {
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

    const { numberOfTables, urlParams, headerFocus, dispatch } = this.props;
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
      const prettyText = print(sdlParse(currentText));
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

    const deriveActionFromOperation = () => {
      const { schema, query } = graphiqlContext.state;
      if (!schema) return;
      if (!query) return;
      let derivedOperationMetadata;
      try {
        derivedOperationMetadata = deriveAction(query.trim(), schema);
      } catch (e) {
        dispatch(showErrorNotification('Unable to derive mutation', e.message));
        console.error(e);
        return;
      }
      const { action, types, variables } = derivedOperationMetadata;
      const actionsSdl = getActionDefinitionSdl(
        action.name,
        action.type,
        action.arguments,
        action.output_type
      );
      if (variables && !variables.length) {
        const ok = getConfirmation(
          `Looks like your ${action.type} does not have variables. This means that the derived action will have no arguments.`
        );
        if (!ok) return;
      }
      const typesSdl = getTypesSdl(types);
      dispatch(
        setActionDefinition(actionsSdl, null, null, sdlParse(actionsSdl))
      );
      dispatch(setTypeDefinition(typesSdl, null, null, sdlParse(typesSdl)));
      dispatch(setDerivedActionParentOperation(query.trim()));
      dispatch(push(getActionsCreateRoute()));
    };

    const renderGraphiql = graphiqlProps => {
      const voyagerUrl = graphqlNetworkData.consoleUrl + '/voyager-view';
      let analyzerProps = {};
      if (graphiqlContext) {
        analyzerProps = graphiqlContext.state;
      }

      // get toolbar buttons
      const getGraphiqlButtons = () => {
        const buttons = [
          {
            label: 'Prettify',
            title: 'Prettify Query (Shift-Ctrl-P)',
            onClick: handleClickPrettifyButton,
          },
          {
            label: 'History',
            title: 'Show History',
            onClick: handleToggleHistory,
          },
          {
            label: this.state.copyButtonText,
            title: 'Copy Query',
            onClick: handleCopyQuery,
          },
          {
            label: 'Explorer',
            title: 'Toggle Explorer',
            onClick: graphiqlProps.toggleExplorer,
          },
          {
            label: 'Voyager',
            title: 'GraphQL Voyager',
            onClick: () => window.open(voyagerUrl, '_blank'),
            icon: <i className="fa fa-external-link" aria-hidden="true" />,
          },
          {
            label: 'Derive action',
            title: 'Derive action for the given mutation',
            onClick: deriveActionFromOperation,
          },
        ];
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
          className={`react-container-graphql ${styles.wd100} ${styles.graphQLHeight} ${styles.box_shadow}`}
        >
          <OneGraphExplorer
            renderGraphiql={renderGraphiql}
            endpoint={graphqlNetworkData.url}
            headers={graphqlNetworkData.headers}
            headersInitialised={graphqlNetworkData.headersInitialised}
            headerFocus={headerFocus}
            urlParams={urlParams}
            numberOfTables={numberOfTables}
            dispatch={dispatch}
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
