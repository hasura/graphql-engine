import React from 'react';
import { LocalEventTriggerState } from '../state';
import styles from '../TableCommon/EventTable.scss';
import Headers, { Header } from '../../../../Common/Headers/Headers';
import CollapsibleToggle from '../../../../Common/CollapsibleToggle/CollapsibleToggle';
import RetryConfEditor from '../../Common/Components/RetryConfEditor';
import * as tooltip from '../Common/Tooltips';
import { Operations } from '../Common/Operations';
import { DataSource } from '../../../../../metadata/types';
import { getSupportedDrivers } from '../../../../../dataSources';
import { DatabaseInfo } from './Add';
import {
  ETOperationColumn,
  EventTriggerOperation,
  RetryConf,
} from '../../types';
import ColumnList from '../Common/ColumnList';
import FormLabel from './FormLabel';
import DebouncedDropdownInput from '../Common/DropdownWrapper';

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
    },
    databaseInfo,
    dataSourcesList,
    readOnlyMode,
    handleTriggerNameChange,
    handleDatabaseChange,
    handleSchemaChange,
    handleTableChange,
    handleWebhookTypeChange,
    handleWebhookValueChange,
    handleOperationsChange,
    handleOperationsColumnsChange,
    handleRetryConfChange,
    handleHeadersChange,
    handleToggleAllColumn,
  } = props;

  const supportedDrivers = getSupportedDrivers('events.triggers.add');
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
        className={`${styles.tableNameInput} form-control`}
        value={name}
        onChange={handleTriggerNameChange}
        maxLength={42}
      />
      <hr className="my-md" />
      <FormLabel title="Database" tooltip={tooltip.triggerNameSource} />
      <select
        className={`${styles.select} form-control ${styles.add_pad_left}`}
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
      <select
        onChange={handleSchemaChange}
        data-test="select-schema"
        className={`${styles.selectTrigger} form-control`}
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
        className={`${styles.selectTrigger} form-control ${styles.add_mar_left}`}
        value={table.name}
      >
        <option value="">Select table</option>
        {databaseInfo[table.schema] &&
          Object.keys(databaseInfo[table.schema])
            .sort()
            .map(t => {
              return (
                <option key={t} value={t}>
                  {t}
                </option>
              );
            })}
      </select>
      <hr className="my-md" />
      <div className={`${styles.add_mar_bottom} ${styles.selectOperations}`}>
        <div className={`${styles.add_mar_bottom} ${styles.selectOperations}`}>
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
      <div className={styles.add_mar_bottom}>
        <FormLabel
          title="Webhook (HTTP/S) Handler"
          tooltip={tooltip.webhookUrlDescription}
        />
        <div>
          <div className={styles.dropdown_wrapper}>
            <DebouncedDropdownInput
              dropdownOptions={[
                { display_text: 'URL', value: 'static' },
                { display_text: 'From env var', value: 'env' },
              ]}
              title={webhook.type === 'static' ? 'URL' : 'From env var'}
              dataKey={webhook.type === 'static' ? 'static' : 'env'}
              onButtonChange={handleWebhookTypeChange}
              onHandlerValChange={handleWebhookValueChange}
              required
              bsClass={styles.dropdown_button}
              handlerVal={webhook.value}
              id="webhook-url"
              inputPlaceHolder={
                webhook.type === 'static'
                  ? 'http://httpbin.org/post'
                  : 'MY_WEBHOOK_URL'
              }
              testId="webhook"
            />
          </div>
        </div>
        <br />
        <small>
          Note: Specifying the webhook URL via an environmental variable is
          recommended if you have different URLs for multiple environments.
        </small>
      </div>
      <hr className="my-md" />
      <CollapsibleToggle
        title={<h4 className={styles.subheading_text}>Advanced Settings</h4>}
        testId="advanced-settings"
      >
        <div>
          <div>
            <FormLabel
              title="Listen columns for update"
              tooltip={tooltip.advancedOperationDescription}
            />
            {operations.update ? (
              <div
                className={`${styles.clear_fix} ${styles.listenColumnWrapper}`}
              >
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
              <div
                className={`${styles.clear_fix} ${styles.listenColumnWrapper}`}
              >
                <i>Applicable only if update operation is selected.</i>
              </div>
            )}
          </div>
          <hr className="my-md" />
          <div className={styles.add_mar_top}>
            <h4 className={styles.subheading_text}>Retry Logic</h4>
            <RetryConfEditor
              retryConf={retryConf}
              setRetryConf={handleRetryConfChange}
              legacyTooltip={false}
            />
          </div>
          <hr className="my-md" />
          <div className={styles.add_mar_top}>
            <h4 className={styles.subheading_text}>Headers</h4>
            <Headers headers={headers} setHeaders={handleHeadersChange} />
          </div>
        </div>
      </CollapsibleToggle>
      <hr className="my-md" />
    </>
  );
};

export default CreateETForm;
