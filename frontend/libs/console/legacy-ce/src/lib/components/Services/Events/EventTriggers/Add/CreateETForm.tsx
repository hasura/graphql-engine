/* eslint-disable no-underscore-dangle */
import React from 'react';
import { Collapsible } from '../../../../../new-components/Collapsible';
import { isProConsole } from '../../../../../utils/proConsole';
import { useSchemas } from '../../../Data/TableInsertItem/hooks/useSchemas';
import { LocalEventTriggerState } from '../state';
import Headers, { Header } from '../../../../Common/Headers/Headers';
import RetryConfEditor from '../../Common/Components/RetryConfEditor';
import * as tooltip from '../Common/Tooltips';
import { Operations } from '../Common/Operations';
import { DataSource } from '../../../../../metadata/types';
import { getSupportedDrivers } from '../../../../../dataSources';
import {
  DatabaseInfo,
  ETOperationColumn,
  EventTriggerOperation,
  RetryConf,
  EventTriggerAutoCleanup,
} from '../../types';
import ColumnList from '../Common/ColumnList';
import FormLabel from './FormLabel';
import { inputStyles, heading } from '../../constants';
import { AutoCleanupForm } from '../Common/AutoCleanupForm';
import { FaShieldAlt } from 'react-icons/fa';

type CreateETFormProps = {
  state: LocalEventTriggerState;
  databaseInfo: DatabaseInfo;
  dataSourcesList: DataSource[];
  readOnlyMode: boolean;
  handleTriggerNameChange: (e: React.ChangeEvent<HTMLInputElement>) => void;
  handleWebhookValueChange: (v: string) => void;
  handleWebhookTypeChange: (e: React.BaseSyntheticEvent) => void;
  handleTableChange: (e: React.ChangeEvent<HTMLSelectElement>) => void;
  handleSchemaChange: (e: React.ChangeEvent<HTMLSelectElement>) => void;
  handleDatabaseChange: (e: React.ChangeEvent<HTMLSelectElement>) => void;
  handleOperationsChange: (o: Record<EventTriggerOperation, boolean>) => void;
  handleOperationsColumnsChange: (oc: ETOperationColumn[]) => void;
  handleRetryConfChange: (r: RetryConf) => void;
  handleHeadersChange: (h: Header[]) => void;
  handleToggleAllColumn: () => void;
  handleAutoCleanupChange: (config: EventTriggerAutoCleanup) => void;
};

const CreateETForm: React.FC<CreateETFormProps> = props => {
  const {
    state: {
      name,
      source,
      table,
      webhook,
      headers,
      retryConf,
      operations,
      operationColumns,
      isAllColumnChecked,
      cleanupConfig,
    },
    databaseInfo,
    dataSourcesList,
    readOnlyMode,
    handleTriggerNameChange,
    handleDatabaseChange,
    handleSchemaChange,
    handleTableChange,
    handleWebhookValueChange,
    handleOperationsChange,
    handleOperationsColumnsChange,
    handleRetryConfChange,
    handleHeadersChange,
    handleToggleAllColumn,
    handleAutoCleanupChange,
  } = props;

  const supportedDrivers = getSupportedDrivers('events.triggers.add');

  const { data: schemas } = useSchemas({
    dataSourceName: source,
    schemaName: table.schema,
  });

  // filter out VIEW from select table dropdown (as we don't support event trigger for VIEW)
  const filterSchemas =
    schemas &&
    schemas.filter((i: { table_type: string }) => {
      return i.table_type !== 'VIEW';
    });

  return (
    <>
      <FormLabel
        title="Trigger Name"
        tooltip={tooltip.triggerNameDescription}
      />
      <input
        type="text"
        data-test="trigger-name"
        placeholder="trigger_name"
        required
        pattern="^[A-Za-z]+[A-Za-z0-9_\\-]*$"
        className={`w-72 ${inputStyles}`}
        value={name}
        onChange={handleTriggerNameChange}
        maxLength={42}
      />
      <hr className="my-md" />
      <FormLabel title="Database" tooltip={tooltip.triggerNameSource} />
      <select
        className={`${inputStyles} pl-md w-72`}
        onChange={handleDatabaseChange}
        data-test="select-source"
        value={source}
      >
        <option value="">Select database</option>
        {dataSourcesList
          .filter(s => supportedDrivers.includes(s.driver))
          .map(s => (
            <option key={s.name} value={s.name}>
              {s.name}
            </option>
          ))}
      </select>
      <hr className="my-md" />
      <FormLabel title="Schema/Table" tooltip={tooltip.postgresDescription} />
      <div className="flex">
        <select
          onChange={handleSchemaChange}
          data-test="select-schema"
          className={`${inputStyles} w-72`}
          value={table.schema}
        >
          <option value="">Select schema</option>
          {Object.keys(databaseInfo)
            .sort()
            .map(s => (
              <option value={s} key={s}>
                {s}
              </option>
            ))}
        </select>
        <select
          onChange={handleTableChange}
          data-test="select-table"
          required
          className={`${inputStyles} w-72 ml-md`}
          value={table.name}
        >
          <option value="">Select table</option>
          {filterSchemas &&
            filterSchemas.sort().map((t: { table_name: string }) => {
              return (
                <option key={t.table_name} value={t.table_name}>
                  {t.table_name}
                </option>
              );
            })}
        </select>
      </div>
      <hr className="my-md" />
      <div className="mb-md">
        <div className="mb-md cursor-pointer">
          <FormLabel
            title="Trigger Operations"
            tooltip={tooltip.operationsDescription}
          />
          <Operations
            selectedOperations={operations}
            setOperations={handleOperationsChange}
            readOnly={false}
            tableName={table.name}
          />
        </div>
      </div>
      <hr className="my-md" />
      <div className="mb-md">
        <FormLabel
          title="Webhook (HTTP/S) Handler"
          tooltip={tooltip.webhookUrlDescription}
          tooltipIcon={
            <FaShieldAlt className="h-4 text-muted cursor-pointer" />
          }
          knowMoreLink="https://hasura.io/docs/latest/api-reference/syntax-defs/#webhookurl"
        />
        <div>
          <div className="w-1/2">
            <p className="text-sm text-gray-600 mb-sm">
              Note: Provide an URL or use an env var to template the handler URL
              if you have different URLs for multiple environments.
            </p>
            <input
              type="text"
              name="handler"
              onChange={e => handleWebhookValueChange(e.target.value)}
              required
              value={webhook.value}
              id="webhook-url"
              placeholder="http://httpbin.org/post or {{MY_WEBHOOK_URL}}/handler"
              data-test="webhook"
              className={`w-82 ${inputStyles}`}
            />
          </div>
        </div>
        <br />
      </div>
      <hr className="my-md" />
      {isProConsole(window.__env) && (
        <>
          <div className="mb-md">
            <div className="mb-md cursor-pointer">
              <AutoCleanupForm
                cleanupConfig={cleanupConfig}
                onChange={handleAutoCleanupChange}
              />
            </div>
          </div>
          <hr className="my-md" />
        </>
      )}
      <Collapsible
        triggerChildren={
          <h2 className="text-lg font-semibold mb-xs flex items-center mb-0">
            Advanced Settings
          </h2>
        }
      >
        <div>
          <div>
            <FormLabel
              title="Listen columns for update"
              tooltip={tooltip.advancedOperationDescription}
            />
            {operations.update ? (
              <div className="clear-both w-72">
                <ColumnList
                  operationColumns={operationColumns}
                  table={table}
                  isAllColumnChecked={isAllColumnChecked}
                  readOnlyMode={readOnlyMode}
                  handleToggleAllColumn={handleToggleAllColumn}
                  handleOperationsColumnsChange={handleOperationsColumnsChange}
                />
              </div>
            ) : (
              <div className="clear-both w-80">
                <i>Applicable only if update operation is selected.</i>
              </div>
            )}
          </div>
          <hr className="my-md" />
          <div className="mt-md">
            <h4 className={heading}>Retry Logic</h4>
            <RetryConfEditor
              retryConf={retryConf}
              setRetryConf={handleRetryConfChange}
              legacyTooltip={false}
            />
          </div>
          <hr className="my-md" />
          <div className="mt-md">
            <h4 className={heading}>Headers</h4>
            <Headers headers={headers} setHeaders={handleHeadersChange} />
          </div>
        </div>
      </Collapsible>
      <hr className="my-md" />
    </>
  );
};

export default CreateETForm;
