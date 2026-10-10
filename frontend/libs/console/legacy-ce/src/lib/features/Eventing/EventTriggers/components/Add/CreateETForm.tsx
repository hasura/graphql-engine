import React from 'react';
import {
  Collapsible,
  FieldLabel,
  HeadersInput,
  Input,
  Select,
  Text,
  Separator,
} from '@hasura/shared/ui';
import { Flex, Heading } from '@radix-ui/themes';
import { LocalEventTriggerState } from '../../types';
import RetryConfEditor from '../form/RetryConfEditor';
import { Operations } from '../form/Operations';
import {
  EventTriggerOperation,
  RetryConf,
  EventTriggerAutoCleanup,
} from '../../types';
import ColumnList from '../form/ColumnList';
import {
  triggerNameDescription,
  triggerNameSource,
  postgresDescription,
  operationsDescription,
  webhookUrlDescription,
} from '../../constants';
import { AutoCleanupForm } from '../form/AutoCleanupForm';
import { FaShieldAlt } from 'react-icons/fa';
import type { EELiteAccessStatus } from '../../../../EETrial';
import { Source, ClientHeader, Table } from '@hasura/shared/types';
import { TableColumn } from '@hasura/metadata/data-source';

import { getTableDisplayName } from '@hasura/shared/utils';
type CreateETFormProps = {
  state: LocalEventTriggerState;
  currentSource: Source | undefined;
  dataSourcesList: Source[];
  readOnlyMode: boolean;
  columns: TableColumn[] | undefined;
  areColumnsFetching: boolean;
  columnsFetchingError: unknown;
  handleTriggerNameChange: (e: React.ChangeEvent<HTMLInputElement>) => void;
  handleWebhookValueChange: (v: string) => void;
  handleWebhookTypeChange: (e: React.BaseSyntheticEvent) => void;
  handleTableChange: (value: Table) => void;
  handleDatabaseChange: (value: string) => void;
  handleOperationsChange: (o: EventTriggerOperation[]) => void;
  handleRetryConfChange: (r: RetryConf) => void;
  handleHeadersChange: (h: ClientHeader[]) => void;
  handleOperationsColumnsChange: (oc: string[]) => void;
  handleAutoCleanupChange: (config: EventTriggerAutoCleanup) => void;
  autoCleanupSupport: EELiteAccessStatus;
  toggleAllColumnChecked: (value: boolean) => void;
};

const CreateETForm: React.FC<CreateETFormProps> = ({
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
  currentSource,
  dataSourcesList,
  readOnlyMode,
  handleTriggerNameChange,
  handleDatabaseChange,
  handleTableChange,
  handleWebhookValueChange,
  handleOperationsChange,
  handleOperationsColumnsChange,
  handleRetryConfChange,
  handleHeadersChange,
  handleAutoCleanupChange,
  autoCleanupSupport,
  columns,
  areColumnsFetching,
  columnsFetchingError,
  toggleAllColumnChecked,
}) => {
  const tableOptions =
    currentSource?.tables
      .map((t) => {
        return {
          value: getTableDisplayName(t.table),
          label: getTableDisplayName(t.table, '', ' / '),
          table: t.table,
        };
      })
      .sort((a, b) => a.value.localeCompare(b.value)) ?? [];

  return (
    <Flex gap="4" direction="column">
      <FieldLabel label="Trigger Name" tooltip={triggerNameDescription} />
      <div className="md:w-1/2">
        <Input
          type="text"
          placeholder="trigger_name"
          required
          pattern="^[A-Za-z]+[A-Za-z0-9_\\-]*$"
          value={name}
          onChange={handleTriggerNameChange}
        />
      </div>
      <Separator size="4" className="my-2" />
      <FieldLabel label="Database" tooltip={triggerNameSource} />
      <div className="md:w-1/2">
        <Select
          onChange={handleDatabaseChange}
          value={source}
          name="source"
          placeholder="Select database"
          options={dataSourcesList.map((s) => ({
            label: s.name,
            value: s.name,
          }))}
          full
        />
      </div>
      <Separator size="4" className="my-2" />
      <FieldLabel label="Schema/Table" tooltip={postgresDescription} />
      <Flex align="center" gap="4" className="w-1/2">
        <Select
          full
          onChange={(name) => {
            const selectedTable = tableOptions.find(
              (opt) => opt.value === name,
            )?.table;
            if (!selectedTable) {
              return;
            }

            handleTableChange(selectedTable);
          }}
          required
          placeholder="Select table"
          value={table ? getTableDisplayName(table) : undefined}
          name="tableName"
          options={tableOptions}
        />
      </Flex>
      <Separator size="4" className="my-2" />
      <div>
        <div className="mb-4">
          <FieldLabel
            label="Trigger Operations"
            tooltip={operationsDescription}
          />
        </div>
        <Operations
          selectedOperations={operations}
          setOperations={handleOperationsChange}
          readOnly={false}
          table={table}
        />
        <ColumnList
          hasUpdateOperation={operations.includes('UPDATE')}
          areColumnsFetching={areColumnsFetching}
          columns={columns}
          columnsFetchingError={columnsFetchingError}
          selectedColumns={operationColumns}
          table={table}
          isAllColumnChecked={isAllColumnChecked}
          readOnlyMode={readOnlyMode}
          setOperationsColumns={handleOperationsColumnsChange}
          toggleAllColumnChecked={toggleAllColumnChecked}
        />
      </div>
      <Separator size="4" className="my-2" />
      <div>
        <FieldLabel
          label="Webhook (HTTP/S) Handler"
          tooltip={webhookUrlDescription}
          tooltipIcon={
            <FaShieldAlt className="h-4 text-muted cursor-pointer" />
          }
          learnMoreLink="https://hasura.io/docs/latest/api-reference/syntax-defs/#webhookurl"
        />
        <div>
          <div className="w-1/2">
            <div className="mb-4">
              <Text as="div">
                Note: Provide an URL or use an env var to template the handler
                URL if you have different URLs for multiple environments.
              </Text>
            </div>
            <Input
              type="text"
              name="handler"
              onChange={(e) => handleWebhookValueChange(e.target.value)}
              required
              value={webhook.value}
              id="webhook-url"
              placeholder="http://httpbin.org/post or {{MY_WEBHOOK_URL}}/handler"
              data-test="webhook"
            />
          </div>
        </div>
        <br />
      </div>
      <Separator size="4" className="my-2" />
      {autoCleanupSupport !== 'forbidden' && (
        <>
          <div className="mb-4">
            <div className="mb-4 cursor-pointer">
              <AutoCleanupForm
                cleanupConfig={cleanupConfig}
                onChange={handleAutoCleanupChange}
              />
            </div>
          </div>
          <Separator size="4" />
        </>
      )}
      <Collapsible
        triggerChildren={<Heading size="4">Advanced Settings</Heading>}
      >
        <div>
          <div>
            <Heading size="4">Retry Logic</Heading>
            <RetryConfEditor
              retryConf={retryConf}
              setRetryConf={handleRetryConfChange}
              legacyTooltip={false}
            />
          </div>
          <Separator size="4" className="my-4" />
          <div className="mt-4">
            <Heading size="4">Headers</Heading>
            <div className="w-8/12 mt-4">
              <HeadersInput
                headers={headers}
                setHeaders={handleHeadersChange}
              />
            </div>
          </div>
        </div>
      </Collapsible>
    </Flex>
  );
};

export default CreateETForm;
