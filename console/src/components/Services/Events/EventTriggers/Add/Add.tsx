import React, { useState, useEffect, useReducer } from 'react';
import { connect, ConnectedProps } from 'react-redux';
import Helmet from 'react-helmet';
import {
  getEventRequestTransformDefaultState,
  requestTransformReducer,
  setEnvVars,
  setSessionVars,
  setRequestMethod,
  setRequestUrl,
  setRequestUrlError,
  setRequestUrlPreview,
  setRequestQueryParams,
  setRequestAddHeaders,
  setRequestBody,
  setRequestBodyError,
  setRequestSampleInput,
  setRequestTransformedBody,
  setEnableRequestBody,
  setRequestContentType,
  setRequestUrlTransform,
  setRequestPayloadTransform,
} from '@/components/Common/ConfigureTransformation/requestTransformState';
import {
  RequestTransformContentType,
  RequestTransformMethod,
} from '@/metadata/types';
import { KeyValuePair } from '@/components/Common/ConfigureTransformation/stateDefaults';
import ConfigureTransformation from '@/components/Common/ConfigureTransformation/ConfigureTransformation';
import {
  getValidateTransformOptions,
  parseValidateApiData,
} from '@/components/Common/ConfigureTransformation/utils';
import requestAction from '@/utils/requestAction';
import Endpoints from '@/Endpoints';
import { Button } from '@/new-components/Button';
import { Table } from '@/dataSources/types';
import { MapStateToProps } from '../../../../../types';
import { useEventTrigger } from '../state';
import styles from '../TableCommon/EventTable.scss';
import { Header } from '../../../../Common/Headers/Headers';
import { createEventTrigger } from '../../ServerIO';
import { EVENTS_SERVICE_HEADING } from '../../constants';
import { mapDispatchToPropsEmpty } from '../../../../Common/utils/reactUtils';
import { getDataSources } from '../../../../../metadata/selector';
import { DataSource } from '../../../../../metadata/types';
import {
  getDatabaseSchemasInfo,
  updateSchemaInfo,
} from '../../../Data/DataActions';
import { getSourceDriver } from '../../../Data/utils';
import {
  currentDriver,
  setDriver,
  isFeatureSupported,
  findTable,
  generateTableDef,
} from '../../../../../dataSources';
import { getEventRequestSampleInput } from '../utils';
import CreateETForm from './CreateETForm';
import {
  ETOperationColumn,
  EventTriggerOperation,
  RetryConf,
} from '../../types';

export type DatabaseInfo = {
  [schema_name: string]: { [table_name: string]: string[] };
};

interface Props extends InjectedProps {}

const Add: React.FC<Props> = props => {
  const { state, setState } = useEventTrigger();
  const {
    name,
    table,
    webhook,
    retryConf,
    source,
    operationColumns,
    operations,
    isAllColumnChecked,
  } = state;
  const {
    dispatch,
    readOnlyMode,
    dataSourcesList,
    currentDataSource,
    allSchemas,
  } = props;

  useEffect(() => {
    if (table.schema) {
      dispatch(
        updateSchemaInfo({
          schemas: [table.schema],
        })
      );
    }
  }, [table.schema]);

  useEffect(() => {
    setState.source(currentDataSource);
    const driver = getSourceDriver(dataSourcesList, currentDataSource);
    if (isFeatureSupported('events.triggers.enabled')) {
      setDriver(driver);
    }
  }, [currentDataSource, dataSourcesList]);

  const [databaseInfo, setDatabaseInfo] = useState<DatabaseInfo>({});

  useEffect(() => {
    dispatch(
      getDatabaseSchemasInfo(currentDriver, source || currentDataSource) as any
    ).then(setDatabaseInfo);
  }, [currentDataSource, source, dispatch]);

  useEffect(() => {
    if (source && table.schema && table.name) {
      const tableDef = findTable(
        allSchemas,
        generateTableDef(table.name, table.schema)
      );
      if (tableDef?.columns) {
        setState.operationColumns(
          tableDef?.columns?.map(c => ({
            name: c.column_name,
            enabled: true,
            type: c.data_type,
          }))
        );
      }
    }
  }, [table.name, source, table.schema, databaseInfo, allSchemas]);

  const [transformState, transformDispatch] = useReducer(
    requestTransformReducer,
    getEventRequestTransformDefaultState()
  );

  const resetSampleInput = () => {
    const value = getEventRequestSampleInput(
      name,
      table.name,
      table.schema,
      retryConf.num_retries,
      operationColumns,
      operations,
      isAllColumnChecked
    );
    transformDispatch(setRequestSampleInput(value));
  };

  const envVarsOnChange = (envVars: KeyValuePair[]) => {
    transformDispatch(setEnvVars(envVars));
  };

  const sessionVarsOnChange = (sessionVars: KeyValuePair[]) => {
    transformDispatch(setSessionVars(sessionVars));
  };

  const requestMethodOnChange = (requestMethod: RequestTransformMethod) => {
    transformDispatch(setRequestMethod(requestMethod));
  };

  const requestUrlOnChange = (requestUrl: string) => {
    transformDispatch(setRequestUrl(requestUrl));
  };

  const requestUrlErrorOnChange = (requestUrlError: string) => {
    transformDispatch(setRequestUrlError(requestUrlError));
  };

  const requestUrlPreviewOnChange = (requestUrlPreview: string) => {
    transformDispatch(setRequestUrlPreview(requestUrlPreview));
  };

  const requestQueryParamsOnChange = (requestQueryParams: KeyValuePair[]) => {
    transformDispatch(setRequestQueryParams(requestQueryParams));
  };

  const requestAddHeadersOnChange = (requestAddHeaders: KeyValuePair[]) => {
    transformDispatch(setRequestAddHeaders(requestAddHeaders));
  };

  const requestBodyOnChange = (requestBody: string) => {
    transformDispatch(setRequestBody(requestBody));
  };

  const requestBodyEnabledOnChange = (enableRequestBody: boolean) => {
    transformDispatch(setEnableRequestBody(enableRequestBody));
  };

  const requestBodyErrorOnChange = (requestBodyError: string) => {
    transformDispatch(setRequestBodyError(requestBodyError));
  };

  const requestSampleInputOnChange = (requestSampleInput: string) => {
    transformDispatch(setRequestSampleInput(requestSampleInput));
  };

  const requestTransformedBodyOnChange = (requestTransformedBody: string) => {
    transformDispatch(setRequestTransformedBody(requestTransformedBody));
  };

  const requestContentTypeOnChange = (
    requestContentType: RequestTransformContentType
  ) => {
    transformDispatch(setRequestContentType(requestContentType));
  };

  const requestUrlTransformOnChange = (data: boolean) => {
    transformDispatch(setRequestUrlTransform(data));
  };

  const requestPayloadTransformOnChange = (data: boolean) => {
    transformDispatch(setRequestPayloadTransform(data));
  };

  useEffect(() => {
    requestUrlErrorOnChange('');
    requestUrlPreviewOnChange('');
    const onResponse = (data: Record<string, any>) => {
      parseValidateApiData(
        data,
        requestUrlErrorOnChange,
        requestUrlPreviewOnChange
      );
    };
    const options = getValidateTransformOptions({
      version: transformState.version,
      inputPayloadString: transformState.requestSampleInput,
      webhookUrl: webhook.value,
      envVarsFromContext: transformState.envVars,
      sessionVarsFromContext: transformState.sessionVars,
      requestUrl: transformState.requestUrl,
      queryParams: transformState.requestQueryParams,
      isEnvVar: webhook.type === 'env',
    });
    if (!webhook.value) {
      requestUrlErrorOnChange(
        'Please configure your webhook handler to generate request url transform'
      );
    } else {
      dispatch(
        requestAction(
          Endpoints.metadata,
          options,
          undefined,
          undefined,
          true,
          true
        )
      ).then(onResponse, onResponse); // parseValidateApiData will parse both success and error
    }
  }, [
    transformState.requestSampleInput,
    webhook,
    transformState.requestUrl,
    transformState.requestQueryParams,
    transformState.envVars,
    transformState.sessionVars,
  ]);

  useEffect(() => {
    requestBodyErrorOnChange('');
    requestTransformedBodyOnChange('');
    const onResponse = (data: Record<string, any>) => {
      parseValidateApiData(
        data,
        requestBodyErrorOnChange,
        undefined,
        requestTransformedBodyOnChange
      );
    };
    const options = getValidateTransformOptions({
      version: transformState.version,
      inputPayloadString: transformState.requestSampleInput,
      webhookUrl: webhook.value,
      envVarsFromContext: transformState.envVars,
      sessionVarsFromContext: transformState.sessionVars,
      transformerBody: transformState.requestBody,
      isEnvVar: webhook.type === 'env',
    });
    if (!webhook.value) {
      requestBodyErrorOnChange(
        'Please configure your webhook handler to generate request body transform'
      );
    } else if (transformState.requestBody && webhook.value) {
      dispatch(
        requestAction(
          Endpoints.metadata,
          options,
          undefined,
          undefined,
          true,
          true
        )
      ).then(onResponse, onResponse); // parseValidateApiData will parse both success and error
    }
  }, [
    transformState.requestSampleInput,
    transformState.requestBody,
    webhook,
    transformState.envVars,
    transformState.sessionVars,
  ]);

  const createBtnText = 'Create Event Trigger';

  const submit = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    dispatch(createEventTrigger(state, transformState));
  };

  const handleTriggerNameChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const triggerName = e.target.value;
    setState.name(triggerName);
  };

  const handleDatabaseChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
    const database = e.target.value;
    setState.source(database);
    dispatch(getDatabaseSchemasInfo('postgres', database) as any).then(
      setDatabaseInfo
    );
  };

  const handleSchemaChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
    const selectedSchemaName = e.target.value;
    setState.table(undefined, selectedSchemaName);
  };

  const handleTableChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
    const selectedTableName = e.target.value;
    setState.table(selectedTableName);
  };

  const handleWebhookTypeChange = (e: React.BaseSyntheticEvent) => {
    const type = e.target.getAttribute('value');
    setState.webhook({
      type,
      value: '',
    });
  };

  const handleWebhookValueChange = (value: string) => {
    setState.webhook({
      type: webhook.type,
      value,
    });
  };

  const handleOperationsChange = (
    o: Record<EventTriggerOperation, boolean>
  ) => {
    setState.operations(o);
  };

  const handleOperationsColumnsChange = (oc: ETOperationColumn[]) => {
    setState.operationColumns(oc);
  };

  const handleRetryConfChange = (r: RetryConf) => {
    setState.retryConf(r);
  };

  const handleHeadersChange = (h: Header[]) => {
    setState.headers(h);
  };

  return (
    <div className="w-full overflow-y-auto bg-gray-50">
      <div className="max-w-6xl">
        <div
          className={`${styles.addTablesBody} ${styles.clear_fix} ${styles.padd_left}`}
        >
          <Helmet
            title={`Add Event Trigger | ${EVENTS_SERVICE_HEADING} - Hasura`}
          />
          <div className={styles.subHeader}>
            <h2 className={styles.heading_text}>Create a new event trigger</h2>
            <div className="clearfix" />
          </div>
          <br />
          <div className={`container-fluid ${styles.padd_left_remove}`}>
            <form onSubmit={submit}>
              <div
                className={`${styles.addCol} col-xs-12 ${styles.padd_left_remove}`}
              >
                <CreateETForm
                  state={state}
                  databaseInfo={databaseInfo}
                  dataSourcesList={dataSourcesList}
                  readOnlyMode={readOnlyMode}
                  handleTriggerNameChange={handleTriggerNameChange}
                  handleWebhookValueChange={handleWebhookValueChange}
                  handleWebhookTypeChange={handleWebhookTypeChange}
                  handleTableChange={handleTableChange}
                  handleSchemaChange={handleSchemaChange}
                  handleDatabaseChange={handleDatabaseChange}
                  handleOperationsChange={handleOperationsChange}
                  handleOperationsColumnsChange={handleOperationsColumnsChange}
                  handleRetryConfChange={handleRetryConfChange}
                  handleHeadersChange={handleHeadersChange}
                  handleToggleAllColumn={setState.toggleAllColumnChecked}
                />
                <ConfigureTransformation
                  state={transformState}
                  resetSampleInput={resetSampleInput}
                  envVarsOnChange={envVarsOnChange}
                  sessionVarsOnChange={sessionVarsOnChange}
                  requestMethodOnChange={requestMethodOnChange}
                  requestUrlOnChange={requestUrlOnChange}
                  requestQueryParamsOnChange={requestQueryParamsOnChange}
                  requestAddHeadersOnChange={requestAddHeadersOnChange}
                  requestBodyOnChange={requestBodyOnChange}
                  requestBodyEnabledOnChange={requestBodyEnabledOnChange}
                  requestSampleInputOnChange={requestSampleInputOnChange}
                  requestContentTypeOnChange={requestContentTypeOnChange}
                  requestUrlTransformOnChange={requestUrlTransformOnChange}
                  requestPayloadTransformOnChange={
                    requestPayloadTransformOnChange
                  }
                />
                {!readOnlyMode && (
                  <Button
                    type="submit"
                    mode="primary"
                    data-test="trigger-create"
                  >
                    {createBtnText}
                  </Button>
                )}
              </div>
            </form>
          </div>
        </div>
      </div>
    </div>
  );
};

type PropsFromState = {
  readOnlyMode: boolean;
  dataSourcesList: DataSource[];
  allSchemas: Table[];
  currentDataSource: string;
};

const mapStateToProps: MapStateToProps<PropsFromState> = state => {
  return {
    readOnlyMode: state.main.readOnlyMode,
    dataSourcesList: getDataSources(state),
    allSchemas: state.tables.allSchemas,
    currentDataSource: state.tables.currentDataSource,
  };
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);

type InjectedProps = ConnectedProps<typeof connector>;

const AddConnector = connector(Add);
export default AddConnector;
