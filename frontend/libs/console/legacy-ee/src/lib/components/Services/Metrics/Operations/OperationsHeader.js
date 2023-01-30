import React, { useEffect, useState } from 'react';

import { connect } from 'react-redux';
import Toggle from 'react-toggle';

import { UPDATE_LOG_SETTINGS } from '../Actions';
import { ErrorModal } from '../SubscriptionWorkers/ErrorModal';
import { isAdmin as _isAdmin } from '../utils';
import { setMetricConfigAction } from '../../../Main/Actions';

const CONFIGS = {
  RESP: 'RESPONSE',
  VAR: 'VARIABLE',
};

const OperationsHeader = ({
  dispatch,
  privileges,
  metadata,
  setMetricConfig,
  refetchMetadata,
}) => {
  const isAdmin = _isAdmin(privileges);

  const [responseBody, updateResponse] = useState(false);
  const [queryVariables, updateVariables] = useState(false);
  const [loading, setLoading] = useState(false);
  const [configError, setConfigError] = useState(null);
  const [showConfigError, updateConfigError] = useState(false);

  useEffect(() => {
    if (metadata && metadata?.loading !== true) {
      try {
        const metricConfig = metadata?.metrics_config || {};
        dispatch({
          type: UPDATE_LOG_SETTINGS,
          data: metricConfig,
        });
        updateVariables(metricConfig.analyze_query_variables || false);
        updateResponse(metricConfig.analyze_response_body || false);
      } catch (err) {
        setConfigError(JSON.stringify(err, null, 4));
        updateConfigError(true);
      }
    }
  }, [metadata]);

  const toggleConfig = config => () => {
    const variables = {
      analyze_response_body:
        config === CONFIGS.RESP ? !responseBody : responseBody,
      analyze_query_variables:
        config === CONFIGS.VAR ? !queryVariables : queryVariables,
    };
    setLoading(true);
    setMetricConfig(variables)
      .then(
        () => {
          refetchMetadata()
            .then(metaData => {
              setLoading(false);
              if (metaData?.metrics_config) {
                dispatch({
                  type: UPDATE_LOG_SETTINGS,
                  data: metaData?.metrics_config,
                });
              }
            })
            .catch(err => {
              setLoading(false);
              setConfigError(err.error);
              updateConfigError(true);
            });
        },
        err => {
          setLoading(false);
          alert(err.toString());
        }
      )
      .catch(err => {
        setLoading(false);
        alert(err.toString());
      });
  };
  const renderToggleOptions = (
    <div className="flex gap-6 mt-1">
      <div className="flex gap-2">
        <div className="text-base font-normal">
          Enable response body analysis
        </div>
        <Toggle
          onChange={toggleConfig(CONFIGS.RESP)}
          checked={responseBody}
          icons={false}
          disabled={loading}
        />
      </div>
      <div className="flex gap-2">
        <div className="text-base font-normal">Capture query variables</div>
        <Toggle
          onChange={toggleConfig(CONFIGS.VAR)}
          checked={queryVariables}
          icons={false}
          disabled={loading}
        />
      </div>
    </div>
  );

  return (
    <div>
      Operations
      {isAdmin && (
        <>
          <div>{renderToggleOptions}</div>
          <div>
            <ErrorModal
              show={showConfigError}
              onHide={() => updateConfigError(false)}
              err={configError}
            />
          </div>
        </>
      )}
    </div>
  );
};
const mapStateToProps = state => {
  return {
    privileges: state?.main?.project?.privileges ?? [],
  };
};
const mapDispatchToProps = dispatch => ({
  setMetricConfig: ({ analyze_query_variables, analyze_response_body }) =>
    dispatch(
      setMetricConfigAction(analyze_query_variables, analyze_response_body)
    ),
  dispatch,
});

export default connect(mapStateToProps, mapDispatchToProps)(OperationsHeader);
