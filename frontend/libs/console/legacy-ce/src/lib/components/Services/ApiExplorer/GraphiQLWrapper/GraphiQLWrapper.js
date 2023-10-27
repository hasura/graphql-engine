/* eslint-disable jsx-a11y/anchor-is-valid */
import React, { Component } from 'react';
import GraphiQL from 'graphiql';
import queryString from 'query-string';
import { connect } from 'react-redux';
import { FaCheckCircle, FaExclamationTriangle } from 'react-icons/fa';
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
import { generateRandomString } from '../../../Services/Data/DataSources/CreateDataSource/utils';
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
import { getQueryResponseCachingRoute } from '../../../../utils/routeUtils';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import {
  setActionDefinition,
  setTypeDefinition,
  setDerivedActionParentOperation,
} from '../../Actions/Add/reducer';
import { getGraphQLEndpoint } from '../utils';
import snippets from './snippets';
import globals from '../../../../Globals';
import { WithEELiteAccess } from '../../../../features/EETrial';
import { isProConsole } from '../../../../utils/proConsole';

import './GraphiQL.css';
import _push from '../../Data/push';
import { isQueryValid } from '../Rest/utils';
import { LS_KEYS, setLSItem } from '../../../../utils/localStorage';
import { CodeExporterEventTracer } from './CodeExporterEventTracer';
import { trackGraphiQlToolbarButtonClick } from '../customAnalyticsEvents';
import {
  ResponseTimeWarning,
  RESPONSE_TIME_CACHE_WARNING,
} from './ResponseTimeWarning';
import { GraphiQLToolbarButton } from './GraphiQLToolbarButton';

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
    trackGraphiQlToolbarButtonClick('Code Exporter');

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
      query: queryFromProps,
      forceIntrospectAt,
    } = this.props;
    const { codeExporterOpen, requestTrackingId } = this.state;
    const graphqlNetworkData = this.props.data;
    const {
      responseTime,
      responseSize,
      isResponseCached,
      responseTrackingId,
      cacheWarning,
      isRequestCachable,
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
      trackGraphiQlToolbarButtonClick('Prettify');

      const editor = graphiqlContext.getQueryEditor();
      const currentText = editor.getValue();
      const prettyText = print(sdlParse(currentText));
      editor.setValue(prettyText);
    };

    const handleToggleHistory = () => {
      trackGraphiQlToolbarButtonClick('History');
      graphiqlContext.setState(prevState => ({
        historyPaneOpen: !prevState.historyPaneOpen,
      }));
    };

    const deriveActionFromOperation = () => {
      trackGraphiQlToolbarButtonClick('Derive action');

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

    const createRouteToREST = gqlProps => () => {
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
      dispatch(_push('/api/rest/create?from=graphiql'));
    };

    const _toggleCacheDirective = () => {
      trackGraphiQlToolbarButtonClick('Cache');

      try {
        const editor = graphiqlContext.getQueryEditor();
        const operationString = editor.getValue();
        const cacheToggledOperationString =
          toggleCacheDirective(operationString);
        editor.setValue(cacheToggledOperationString);
      } catch {
        // throw a generic error
        throw new Error(
          'Caching directives can only be added to valid GraphQL queries.'
        );
      }
    };

    const _addCacheDirective = () => {
      try {
        const editor = graphiqlContext.getQueryEditor();
        const operationString = editor.getValue();
        const cacheToggledOperationString = toggleCacheDirective(
          operationString,
          true
        );
        editor.setValue(cacheToggledOperationString);
      } catch {
        // throw a generic error
        throw new Error(
          'Caching directives can only be added to valid GraphQL queries.'
        );
      }
    };

    const shouldShowResponseTimeWarning =
      isRequestCachable &&
      responseTime > RESPONSE_TIME_CACHE_WARNING &&
      !isResponseCached &&
      !cacheWarning;

    const renderGraphiqlFooter = responseTime &&
      responseTrackingId === requestTrackingId && (
        <GraphiQL.Footer>
          <div className="flex items-center sticky bottom-0 w-full z-[100] p-sm bg-[#eeeeee]">
            <span className="text-xs text-[#777777] font-semibold mr-sm uppercase">
              Response Time
            </span>
            <span className="text-sm text-black mr-md">{responseTime} ms</span>
            {shouldShowResponseTimeWarning && (
              <ResponseTimeWarning onAddCacheDirective={_addCacheDirective} />
            )}
            {responseSize && (
              <>
                <span className="text-xs text-[#777777] mr-sm uppercase font-semibold">
                  Response Size
                </span>
                <span className="text-sm text-black mr-md">
                  {responseSize} bytes
                </span>
              </>
            )}
            {isResponseCached && (
              <>
                <span className="text-xs text-[#777777] font-semibold mr-sm uppercase">
                  Cached
                </span>
                <ToolTip
                  message="This query response was cached using the @cached directive"
                  placement="top"
                  tooltipStyle="text-[#777] ml-sm mr-xs"
                />
                <FaCheckCircle className="text-[#008000]" />
              </>
            )}
            {!isResponseCached && cacheWarning && (
              <>
                <span className="text-xs text-[#777777] font-semibold uppercase">
                  Not Cached
                </span>
                <ToolTip
                  message={`Response not cached due to: "${cacheWarning}"`}
                  placement="top"
                  tooltipStyle="text-yellow-600"
                >
                  <FaExclamationTriangle className="text-yellow-600 mb-1" />
                </ToolTip>
              </>
            )}
          </div>
        </GraphiQL.Footer>
      );

    const renderGraphiql = graphiqlProps => {
      const voyagerUrl = graphqlNetworkData.consoleUrl + '/voyager-view';
      const queryParams = queryString.parseUrl(window.location.href);
      const highlightRestButton = queryParams.query.mode === 'rest';

      let analyzerProps = {};
      if (graphiqlContext) {
        analyzerProps = graphiqlContext.state;
      }

      // get toolbar buttons
      const getGraphiqlButtons = eeLiteAccess => {
        const routeToREST = createRouteToREST(graphiqlProps);

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
            onClick: () => {
              trackGraphiQlToolbarButtonClick('Explorer');
              graphiqlProps.toggleExplorer();
            },
          },
          {
            label: 'Cache',
            title: 'Cache the response of this query',
            onClick: () => {
              if (eeLiteAccess === 'active' || isProConsole(globals)) {
                // toggle cache directive only if it is cloud/ee-classic/ee-lite-active
                _toggleCacheDirective();
              } else {
                // send to the query-response-caching page if the EE lite trial isn't active
                if (eeLiteAccess !== 'forbidden') {
                  dispatch(_push(getQueryResponseCachingRoute()));
                }
              }
            },
            hide: !isProConsole(globals) && eeLiteAccess === 'forbidden',
          },
          {
            label: 'Code Exporter',
            title: 'Toggle Code Exporter',
            onClick: this._handleToggleCodeExporter,
          },
          {
            label: 'REST',
            title: 'REST Endpoints',
            primary: highlightRestButton,
            onClick: () => {
              trackGraphiQlToolbarButtonClick('REST');
              routeToREST();
            },
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
            return <GraphiQLToolbarButton key={b.label} {...b} />;
          });
      };

      return (
        <>
          <CodeExporterEventTracer />
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
              <WithEELiteAccess globals={globals}>
                {({ access: eeLiteAccess }) => {
                  return getGraphiqlButtons(eeLiteAccess);
                }}
              </WithEELiteAccess>
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
        <div className="w-full h-full border overflow-hidden rounded border-gray-300">
          <OneGraphExplorer
            renderGraphiql={renderGraphiql}
            endpoint={getGraphQLEndpoint(mode)}
            headers={graphqlNetworkData.headers}
            headersInitialised={graphqlNetworkData.headersInitialised}
            headerFocus={headerFocus}
            urlParams={urlParams}
            loading={loading}
            numberOfTables={numberOfTables}
            dispatch={dispatch}
            mode={mode}
            forceIntrospectAt={forceIntrospectAt}
            query={queryFromProps}
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
  query: PropTypes.string,
};

const mapStateToProps = state => ({
  mode: state.apiexplorer.mode,
  loading: state.apiexplorer.loading,
  query: state.apiexplorer.graphiql.query,
  forceIntrospectAt: state.apiexplorer.graphiql.forceIntrospectAt,
});

const GraphiQLWrapperConnected = connect(mapStateToProps)(GraphiQLWrapper);

export default GraphiQLWrapperConnected;
