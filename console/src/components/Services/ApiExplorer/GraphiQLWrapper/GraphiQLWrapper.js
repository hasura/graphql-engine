import React, { Component } from 'react';
import GraphiQL from 'graphiql';
import { connect } from 'react-redux';
import { FaCheckCircle } from 'react-icons/fa';
import PropTypes from 'prop-types';
import GraphiQLErrorBoundary from './GraphiQLErrorBoundary';
import OneGraphExplorer from '../OneGraphExplorer/OneGraphExplorer';
import AnalyzeButton from '../Analyzer/AnalyzeButton';
import CodeExporter from 'graphiql-code-exporter';
import {
  getPersistedCodeExporterOpen,
  persistCodeExporterOpen,
} from '../OneGraphExplorer/utils';

import { clearCodeMirrorHints, setQueryVariableSectionHeight } from './utils';
import { generateRandomString } from '../../../Services/Data/DataSources/CreateDataSource/Heroku/utils';
import { analyzeFetcher, graphQLFetcherFinal } from '../Actions';
import { parse as sdlParse, print } from 'graphql';
import deriveAction from '../../../../shared/utils/deriveAction';
import {
  getActionDefinitionSdl,
  getTypesSdl,
  toggleCacheDirective,
} from '../../../../shared/utils/sdlUtils';
import { showErrorNotification } from '../../Common/Notification';
import ToolTip from '../../../Common/Tooltip/Tooltip';
import { getActionsCreateRoute } from '../../../Common/utils/routesUtils';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import {
  setActionDefinition,
  setTypeDefinition,
  setDerivedActionParentOperation,
} from '../../Actions/Add/reducer';
import { getGraphQLEndpoint } from '../utils';
import snippets from './snippets';
import globals from '../../../../Globals';

import 'graphiql/graphiql.css';
import './GraphiQL.css';
import 'graphiql-code-exporter/CodeExporter.css';
import _push from '../../Data/push';
import { isQueryValid } from '../Rest/utils';
import { LS_KEYS, setLSItem } from '@/utils/localStorage';

class GraphiQLWrapper extends Component {
  constructor(props) {
    super(props);
    this.state = {
      error: false,
      noSchema: false,
      onBoardingEnabled: false,
      copyButtonText: 'Copy',
      codeExporterOpen: false,
      requestTrackingId: null,
    };
  }

  componentDidMount() {
    setQueryVariableSectionHeight();
  }

  componentWillMount() {
    const codeExporterOpen = getPersistedCodeExporterOpen();
    this.setState({ codeExporterOpen });
  }

  componentWillUnmount() {
    clearCodeMirrorHints();
  }

  _handleToggleCodeExporter = () => {
    const nextState = !this.state.codeExporterOpen;

    persistCodeExporterOpen(nextState);

    this.setState({ codeExporterOpen: nextState });
  };

  render() {
    const {
      numberOfTables,
      urlParams,
      headerFocus,
      dispatch,
      mode,
      loading,
    } = this.props;
    const { codeExporterOpen, requestTrackingId } = this.state;
    const graphqlNetworkData = this.props.data;
    const {
      responseTime,
      responseSize,
      isResponseCached,
      responseTrackingId,
    } = this.props.response;

    const graphQLFetcher = graphQLParams => {
      if (headerFocus) {
        return null;
      }

      const trackingId = generateRandomString();
      this.setState({ requestTrackingId: trackingId });

      return graphQLFetcherFinal(
        graphQLParams,
        getGraphQLEndpoint(mode),
        graphqlNetworkData.headers,
        dispatch,
        trackingId
      );
    };

    const analyzeFetcherInstance = analyzeFetcher(
      graphqlNetworkData.headers,
      mode
    );

    let graphiqlContext;

    const handleClickPrettifyButton = () => {
      const editor = graphiqlContext.getQueryEditor();
      const currentText = editor.getValue();
      const prettyText = print(sdlParse(currentText));
      editor.setValue(prettyText);
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
      dispatch(_push(getActionsCreateRoute()));
    };

    const routeToREST = gqlProps => () => {
      const { query, schema } = graphiqlContext.state;
      setLSItem(LS_KEYS.graphiqlQuery, query);
      if (!query || !schema || !gqlProps.query || !isQueryValid(query)) {
        dispatch(
          showErrorNotification(
            'Unable to create a REST endpoint',
            'Please enter a valid named GraphQL query or mutation'
          )
        );
        return;
      }
      dispatch(_push('/api/rest/create'));
    };

    const _toggleCacheDirective = () => {
      const editor = graphiqlContext.getQueryEditor();
      const operationString = editor.getValue();
      const cacheToggledOperationString = toggleCacheDirective(operationString);
      editor.setValue(cacheToggledOperationString);
    };

    const renderGraphiqlFooter = responseTime &&
      responseTrackingId === requestTrackingId && (
        <GraphiQL.Footer>
          <div className="graphiql_footer">
            <span className="graphiql_footer_label">Response Time</span>
            <span className="graphiql_footer_value">{responseTime} ms</span>
            {responseSize && (
              <>
                <span className="graphiql_footer_label">Response Size</span>
                <span className="graphiql_footer_value">
                  {responseSize} bytes
                </span>
              </>
            )}
            {isResponseCached && (
              <>
                <span className="graphiql_footer_label">Cached</span>
                <ToolTip
                  message="This query response was cached using the @cached directive"
                  placement="top"
                  tooltipStyle="graphiql_footer_icon"
                />
                <FaCheckCircle className="color_green" />
              </>
            )}
          </div>
        </GraphiQL.Footer>
      );

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
            label: 'Explorer',
            title: 'Toggle Explorer',
            onClick: graphiqlProps.toggleExplorer,
          },
          {
            label: 'Cache',
            title: 'Cache the response of this query',
            onClick: _toggleCacheDirective,
            hide: globals.consoleType !== 'cloud',
          },
          {
            label: 'Code Exporter',
            title: 'Toggle Code Exporter',
            onClick: this._handleToggleCodeExporter,
          },
          {
            label: 'REST',
            title: 'REST Endpoints',
            onClick: routeToREST(graphiqlProps),
          },
        ];
        if (mode === 'graphql') {
          buttons.push({
            label: 'Derive action',
            title: 'Derive action for the given mutation',
            onClick: deriveActionFromOperation,
          });
        }
        return buttons
          .filter(b => !b.hide)
          .map(b => {
            return <GraphiQL.Button key={b.label} {...b} />;
          });
      };

      return (
        <>
          <GraphiQL
            {...graphiqlProps}
            ref={c => {
              graphiqlContext = c;
            }}
            fetcher={graphQLFetcher}
            voyagerUrl={voyagerUrl}
          >
            <GraphiQL.Logo>GraphiQL</GraphiQL.Logo>
            <GraphiQL.Toolbar>
              {getGraphiqlButtons()}
              <AnalyzeButton
                operations={graphiqlContext && graphiqlContext.state.operations}
                analyzeFetcher={analyzeFetcherInstance}
                dispatch={dispatch}
                {...analyzerProps}
              />
            </GraphiQL.Toolbar>
            {renderGraphiqlFooter}
          </GraphiQL>
          {codeExporterOpen ? (
            <CodeExporter
              hideCodeExporter={this._handleToggleCodeExporter}
              snippets={snippets}
              query={graphiqlProps.query}
              codeMirrorTheme="default"
            />
          ) : null}
        </>
      );
    };

    return (
      <GraphiQLErrorBoundary>
        <div
          className={`react-container-graphql w-full h-full border mt-md overflow-hidden rounded border-gray-300`}
        >
          <OneGraphExplorer
            renderGraphiql={renderGraphiql}
            endpoint={getGraphQLEndpoint(mode)}
            dispatch={dispatch}
            headers={graphqlNetworkData.headers}
            headersInitialised={graphqlNetworkData.headersInitialised}
            headerFocus={headerFocus}
            urlParams={urlParams}
            loading={loading}
            numberOfTables={numberOfTables}
            dispatch={dispatch}
            mode={mode}
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
  response: PropTypes.object,
};

const mapStateToProps = state => ({
  mode: state.apiexplorer.mode,
  loading: state.apiexplorer.loading,
});

const GraphiQLWrapperConnected = connect(mapStateToProps)(GraphiQLWrapper);

export default GraphiQLWrapperConnected;
