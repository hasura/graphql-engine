import React from 'react';
import BootstrapModal from 'react-bootstrap/lib/Modal';
// import BootstrapModalButton from 'react-bootstrap/lib/Button';
import CustomCopy from './CustomCopy';

import { transformedVals } from '../Operations/utils';
import styles from '../Metrics.scss';

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

const Modal = props => {
  const { onHide, data, nullData, configData } = props;

  let analyzeResponseBody = true;
  let analyzeVariables = true;

  if (Object.keys(configData).length > 0) {
    analyzeResponseBody = configData.analyze_response_body;
    analyzeVariables = configData.analyze_query_variables;
  }

  const renderSessonVars = () => {
    const { user_vars: userVars } = data;
    const userVarKeys = Object.keys(userVars);
    const sessionVariables = {};
    if (userVars && userVarKeys.length > 0) {
      userVarKeys.map(u => {
        sessionVariables[u] = userVars[u];
      });
    }
    return (
      <div
        className={`${styles.resetInfoWrapperPadd} ${styles.alignedCustomCopy} ${styles.paddingTop}`}
      >
        <CustomCopy
          label="SESSION VARIABLES"
          copy={JSON.stringify(sessionVariables, null, 2)}
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
        <div
          className={`${styles.resetInfoWrapperPadd} ${styles.alignedCustomCopy}
          }`}
        >
          <CustomCopy label="EMPTY ARRAYS & NULLS IN RESPONSE" copy={d} />
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
          <div className={styles.errorMessage}>ERROR:</div>
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
        <CustomCopy label={'OPERATION STRING'} copy={graphQLQuery} />
      );
      renderItem.push(queryElement);
      if (variables && analyzeVariables) {
        try {
          const formattedVar = JSON.stringify(variables, null, 2);
          const variablesElement = [
            <CustomCopy label={'QUERY VARIABLES'} copy={formattedVar} />,
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
          <CustomCopy
            label={'GENERATED SQL'}
            copy={JSON.stringify(generatedSql, null, 2)}
          />
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
      return <CustomCopy label="REQUEST HEADERS" copy={stringified} />;
    } catch (e) {
      console.log(e);
    }
  };

  return (
    <BootstrapModal
      id="operationInspect"
      onHide={onHide}
      show
      size="modal-lg"
      className={styles.modalWrapper}
    >
      <BootstrapModal.Header className={styles.modalHeader} closeButton>
        <BootstrapModal.Title className={styles.title}>
          Inspect
        </BootstrapModal.Title>
      </BootstrapModal.Header>
      <BootstrapModal.Body
        className={styles.modalContainer}
        style={{ maxHeight: '986px' }}
      >
        <div
          className={
            styles.noPadd +
            ' col-md-6 ' +
            styles.borderRight +
            ' ' +
            styles.flexColumn
          }
        >
          <div className={styles.infoWrapper}>
            <div className={`${styles.infoField} ${styles.paddingBottom}`}>
              <div className={styles.information}>
                TIMESTAMP: <span>{new Date(time).toLocaleString()}</span>
              </div>
              {/*
              <div className={styles.information}>
                ID: <span>{id}</span>
              </div>
              */}
              <div className={styles.information}>
                OPERATION NAME: <span>{operationName || 'N/A'}</span>
              </div>
              <div className={styles.information}>
                OPERATION ID: <span>{operationId || 'N/A'}</span>
              </div>
              <div className={styles.information}>
                REQUEST ID: <span>{requestId}</span>
              </div>
              <div
                className={
                  styles.information +
                  ' ' +
                  styles.borderBottom +
                  ' ' +
                  styles.addPaddBottom
                }
              >
                TRANSPORT: <span>{transport}</span>
              </div>
              {transport === 'ws' ? (
                <div className={styles.borderBottom + ' ' + styles.paddingTop}>
                  <div className={styles.information}>
                    WEBSOCKET ID: <span>{websocketId}</span>
                  </div>
                  <div className={styles.information}>
                    WEBSOCKET OPERATION ID: <span>{websocketOperationId}</span>
                  </div>
                  {operationType === 'subscription' ? (
                    <div className={styles.information}>
                      SUBSCRIPTION STATUS <span>{status}</span>
                    </div>
                  ) : null}
                </div>
              ) : null}
            </div>
            <div className={`${styles.infoField} ${styles.noPaddingBottom}`}>
              {/*
              <div className={styles.information}>
                CLIENT ID: <span>{clientId}</span>
              </div>
              */}
              <div className={styles.information}>
                CLIENT NAME: <span>{client_name || 'N/A'}</span>
              </div>
              <div
                className={`${styles.information} ${styles.noPaddingBottom}`}
              >
                ROLE: <span>{role || 'N/A'}</span>
              </div>
            </div>
          </div>
          {renderSessonVars()}
          <div className={`${styles.infoWrapper} ${styles.noPaddingTop}`}>
            <div className={`${styles.infoField} ${styles.noPaddingBottom}`}>
              <div
                className={`
                  ${styles.information} ${styles.borderTop} ${styles.paddingTop}
                `}
              >
                EXECUTION TIME:{' '}
                <span>
                  {('execution_time' in transformedVals &&
                    transformedVals.execution_time(executionTime)) ||
                    executionTime}{' '}
                  ms
                </span>
              </div>
              <div
                className={`
                  ${styles.information} ${styles.paddingTop}
                `}
              >
                TIMING
                <div className={styles.paddingTop}>
                  {trace && trace?.length && <TraceGraph trace={trace} />}
                </div>
              </div>
              <div className={`${styles.information} ${styles.addPaddBottom}`}>
                REQUEST SIZE:{' '}
                <span>
                  {(transformedVals.hasOwnProperty('request_size') &&
                    transformedVals.request_size(requestSize)) ||
                    requestSize}{' '}
                  kB
                </span>
                <br />
                RESPONSE SIZE:{' '}
                <span>
                  {('response_size' in transformedVals &&
                    transformedVals.response_size(responseSize)) ||
                    responseSize}{' '}
                  kB
                </span>
              </div>
            </div>
          </div>
          {renderError()}
          {renderResponseAnalysis()}
        </div>
        <div className={styles.noPadd + ' col-md-6'}>
          {renderOperationQuery()}
          {renderGeneratedSql()}
          {renderRequestHeaders()}
          {/*
          <div className={styles.infoWrapper}>
            <div className={styles.information}>
              GENERATED SQL:{' '}
              <button className={styles.analyzeBtn}>ANALYZE</button>
            </div>
          </div>
          <div className={styles.boxwrapper + ' ' + styles.errorBox}>
            <div className={styles.box} />
          </div>
          */}
        </div>
      </BootstrapModal.Body>
    </BootstrapModal>
  );
};

export default Modal;
