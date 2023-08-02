import clsx from 'clsx';
import CustomCopy from './CustomCopy';
import { Dialog } from '@hasura/console-legacy-ce';

import { transformedVals } from '../Operations/utils';
import styles from '../Metrics.module.scss';

import { FlameGraph } from 'react-flame-graph';

const TraceGraph = props => {
  const { trace } = props;

  const nodes = {};
  const roots = [];

  trace.forEach(t => {
    if (t) {
      nodes[t.span_id] = {
        name: t.name,
        value: t.duration,
        depth: 0,
      };
    }
  });

  trace.forEach(t => {
    if (t) {
      const parentId = t?.parent_id;
      const node = nodes[t?.span_id];
      if (parentId) {
        const parent = nodes[parentId] || {};
        parent.children = (parent?.children || []).concat([node]);
        node.depth = Math.max(node.depth, nodes[parentId]?.depth + 1);

        const nodeTime = node?.value / 1000000;
        const parentTime = parent?.value / 1000000;
        const parentPercent = Math.round((nodeTime / parentTime) * 100);

        const metaKeys = Object.keys(t.meta).filter(k => k !== 'request_id');
        const metadata = metaKeys.map(k => `${k}: ${t.meta[k]}`).join('\n');
        node.tooltip = `${nodeTime}ms (${parentPercent}%)${
          metaKeys?.length > 0 ? '\n\n' + metadata : ''
        }`;
      } else {
        roots.push(node);
      }
    }
  });

  Object.keys(nodes).forEach(k => {
    nodes[k].backgroundColor = nodes[k].depth % 2 === 0 ? '#ddd' : '#eee';
  });

  const root = roots[0];

  return root ? <FlameGraph data={root} height={200} width={375} /> : null;
};

const LabelValue = props => {
  const { label, value, className } = props;
  return (
    <div className={className}>
      <div className={clsx('bg-white rounded p-2', className)}>
        <div className="text-slate-500 text-base mr-1 whitespace-nowrap">
          {label}
        </div>
        <div>
          <strong>{value}</strong>
        </div>
      </div>
    </div>
  );
};

const Modal = props => {
  const { onHide, data, nullData, configData } = props;

  let analyzeResponseBody = true;
  let analyzeVariables = true;

  if (Object.keys(configData).length > 0) {
    analyzeResponseBody = configData.analyze_response_body;
    analyzeVariables = configData.analyze_query_variables;
  }

  const renderSessionVars = () => {
    const { user_vars: userVars } = data;
    const userVarKeys = Object.keys(userVars);
    const sessionVariables = {};
    if (userVars && userVarKeys.length > 0) {
      userVarKeys.map(u => {
        sessionVariables[u] = userVars[u];
      });
    }
    return (
      <div className="rounded bg-white text-sm">
        <CustomCopy
          label={
            <LabelValue className="inline-block" label="Session variables" />
          }
          copy={JSON.stringify(sessionVariables, null, 2)}
          displayColon={false}
          displayAcknowledgement={false}
          contentMaxHeight="200px"
        />
      </div>
    );
  };
  const {
    time,
    // id,
    operation_name: operationName,
    request_id: requestId,
    // server_client_id: clientId,
    user_role: role,
    client_name,
    execution_time: executionTime,
    trace,
    request_size: requestSize,
    response_size: responseSize,
    error: requestError,
    query,
    operation_id: operationId,
    generated_sql: generatedSql,
    request_headers: requestHeaders,
    transport,
    websocket_id: websocketId,
    ws_operation_id: websocketOperationId,
    operation_type: operationType,
    status,
  } = data;

  const spreadIfExists = (_data, key) => {
    if (_data[key]) {
      return { [key]: _data[key] };
    }
    return {};
  };

  const renderResponseAnalysis = () => {
    if (!requestError && analyzeResponseBody) {
      let d = '{}';
      if (nullData) {
        d = JSON.stringify(
          {
            ...spreadIfExists(nullData, 'nulls'),
            ...spreadIfExists(nullData, 'empty_arrays'),
          },
          null,
          2
        );
      }
      if (!configData.analyze_response_body) {
        d = 'Enable response body analysis';
      }
      return (
        <div className="rounded bg-white text-sm">
          <CustomCopy
            label={
              <LabelValue
                className="inline-block"
                label="Empty arrays & nulls in response"
              />
            }
            copy={d}
            displayColon={false}
            displayAcknowledgement={false}
            contentMaxHeight="200px"
          />
        </div>
      );
    }
    return null;
  };

  const renderError = () => {
    if (requestError) {
      const formattedError = JSON.stringify(requestError, null, 2);
      return (
        <div className={styles.boxwrapper + ' ' + styles.errorBox}>
          <LabelValue label="Error:" />
          <div className={styles.errorBox}>
            <code className={styles.queryCode}>
              <pre style={{ whitespace: 'pre-wrap' }}>{formattedError}</pre>
            </code>
          </div>
        </div>
      );
    }
    return null;
  };
  const renderOperationQuery = () => {
    const renderItem = [];
    if (query) {
      const { query: graphQLQuery, variables } = query;
      const queryElement = (
        <div className="rounded bg-white text-sm">
          <CustomCopy
            label={
              <LabelValue className="inline-block" label="Operation string" />
            }
            copy={graphQLQuery}
            displayColon={false}
            displayAcknowledgement={false}
            contentMaxHeight="200px"
          />
        </div>
      );
      renderItem.push(queryElement);
      if (variables && analyzeVariables) {
        try {
          const formattedVar = JSON.stringify(variables, null, 2);
          const variablesElement = [
            <div className="rounded bg-white text-sm">
              <CustomCopy
                label={
                  <LabelValue
                    className="inline-block"
                    label="Query variables"
                  />
                }
                copy={formattedVar}
                displayColon={false}
                displayAcknowledgement={false}
                contentMaxHeight="200px"
              />
            </div>,
          ];
          renderItem.push(variablesElement);
        } catch (e) {
          //
        }
      }
      return renderItem;
    }
    return null;
  };

  const renderGeneratedSql = () => {
    if (generatedSql) {
      try {
        return (
          <div className="rounded bg-white text-sm">
            <CustomCopy
              label={
                <LabelValue className="inline-block" label="Generated SQL" />
              }
              copy={JSON.stringify(generatedSql, null, 2)}
              displayColon={false}
              displayAcknowledgement={false}
              contentMaxHeight="200px"
            />
          </div>
        );
      } catch (e) {
        console.error(e);
      }
    }
    return null;
  };

  const renderRequestHeaders = () => {
    if (!requestHeaders) return null;
    try {
      const stringified = JSON.stringify(requestHeaders, null, 2);
      return (
        <div className="rounded bg-white text-sm">
          <CustomCopy
            label={
              <LabelValue className="inline-block" label="Request headers:" />
            }
            copy={stringified}
            displayColon={false}
            displayAcknowledgement={false}
            contentMaxHeight="200px"
          />
        </div>
      );
    } catch (e) {
      console.log(e);
    }
  };

  return (
    <Dialog
      id="operationInspect"
      onClose={onHide}
      hasBackdrop
      size="xxxl"
      title={
        <div className="font-normal text-slate-900 flex gap-2 text-left w-full">
          <div>
            <div>
              <span className="text-slate-500">Operation </span>
              <strong>{operationName || 'N/A'}</strong>
            </div>
            <div className="text-sm">
              <span className="text-slate-500">Id </span>
              {operationId || 'N/A'}
            </div>
          </div>
          <div className="text-base flex-grow text-right pr-6 pt-2">
            <LabelValue
              label="Timestamp:"
              value={new Date(time).toLocaleString()}
              className="bg-transparent flex justify-end"
            />
          </div>
        </div>
      }
    >
      <div
        className={`border border-top overflow-y-auto flex w-full text-left bg-slate-100`}
      >
        <div className="flex flex-col flex-shrink p-4 pr-2 gap-4 w-1/2">
          <LabelValue label="Request Id" value={requestId} />
          <LabelValue label="Transport" value={transport} />
          {transport === 'ws' ? (
            <>
              <LabelValue label="Websocket Id" value={websocketId} />
              <LabelValue
                label="Websocket operation Id"
                value={websocketOperationId}
              />
              {operationType === 'subscription' ? (
                <LabelValue label="Subscription status" value={status} />
              ) : null}
            </>
          ) : null}
          <LabelValue label="Client name" value={role || 'N/A'} />
          <LabelValue label="Role" value={client_name || 'N/A'} />
          <LabelValue
            label="Request size"
            value={
              ((transformedVals.hasOwnProperty('request_size') &&
                transformedVals.request_size(requestSize)) ||
                requestSize) + ' kB'
            }
          />
          <LabelValue
            label="Response size"
            value={
              (('response_size' in transformedVals &&
                transformedVals.response_size(responseSize)) ||
                responseSize) + ' kB'
            }
          />
          {renderSessionVars()}
          <div>
            <LabelValue
              label="Execution time"
              value={
                (('execution_time' in transformedVals &&
                  transformedVals.execution_time(executionTime)) ||
                  executionTime) + ' ms'
              }
            />
            <LabelValue
              label="Timing"
              value={trace && trace?.length && <TraceGraph trace={trace} />}
            />
          </div>
          {renderError()}
          {renderResponseAnalysis()}
        </div>
        <div className="flex flex-col flex-shrink p-4 gap-4 w-1/2">
          {renderOperationQuery()}
          {renderGeneratedSql()}
          {renderRequestHeaders()}
        </div>
      </div>
    </Dialog>
  );
};

export default Modal;
