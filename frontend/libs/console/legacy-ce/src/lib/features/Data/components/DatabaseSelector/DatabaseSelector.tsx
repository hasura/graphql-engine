import { DataTarget } from '../../../Datasources';
import clsx from 'clsx';
import get from 'lodash/get';
import React, { useState } from 'react';
import { FieldError } from 'react-hook-form';
import { FaDatabase, FaFolder, FaTable } from 'react-icons/fa';
import { useSourcesTree } from './hooks/useSourceTree';
import { Select } from './Select';

interface Props {
  value: DataTarget;
  disabledKeys?: string[];
  onChange?: (value: DataTarget) => void;
  name: string;
  errors?: Record<string, any>;
  className?: string;
  hiddenKeys?: string[];
  labels?: Record<string, string>;
}

export const DatabaseSelector = (props: Props) => {
  const { value, onChange, name, errors, disabledKeys, hiddenKeys, labels } =
    props;
  const maybeError = get(errors, name) as
    | { database?: FieldError; schema?: FieldError; table?: FieldError }
    | undefined;

  const [localValue, setLocalValue] = useState<{
    database: string;
    schema: string;
    table: string;
  }>({
    database: value.database,
    schema: (value as any).schema ?? (value as any).dataset,
    table: value.table,
  });

  const { tree, isLoading } = useSourcesTree();

  const schemaOptions = tree[localValue.database]
    ? Object.keys(tree[localValue.database].children)
    : [];

  const tableOptions = tree[localValue.database]?.children?.[localValue.schema]
    ? tree[localValue.database]?.children?.[localValue.schema]
    : [];

  const updateParent = (val: {
    database: string;
    schema: string;
    table: string;
  }) => {
    if (!onChange) return;

    if (tree[val.database].kind !== 'bigquery')
      onChange({
        database: val.database,
        schema: val.schema,
        table: val.table,
      });
    else
      onChange({
        database: val.database,
        dataset: val.schema,
        table: val.table,
      });
  };

  if (isLoading) return <>Loading data ...</>;

  return (
    <div
      id="source"
      className={clsx(
        `grid grid-cols-12 rounded bg-gray-50 border border-gray-300 p-md gap-y-4 ${props.className}`
      )}
    >
      <div
        className={clsx(
          'col-span-12',
          hiddenKeys?.includes('database') ? 'hidden' : ''
        )}
      >
        <Select
          options={Object.keys(tree)}
          value={localValue.database}
          disabled={!Object.keys(tree) || disabledKeys?.includes('database')}
          onChange={e => {
            e.persist();
            const temp = {
              database: e.target.value,
              schema: '',
              table: '',
            };
            setLocalValue(temp);
            updateParent(temp);
          }}
          placeholder="Select a database..."
          error={maybeError?.database}
          data-testid={`${name}_database`}
          icon={<FaDatabase />}
          label={labels?.database ?? 'Source'}
          name={`${name}_database`}
        />
      </div>

      <div
        className={clsx(
          'col-span-12',
          hiddenKeys?.includes('schema') ? 'hidden' : ''
        )}
      >
        <Select
          options={schemaOptions}
          value={(localValue as any).schema ?? (localValue as any).dataset}
          disabled={!schemaOptions || disabledKeys?.includes('schema')}
          onChange={e => {
            e.persist();
            const temp = {
              ...localValue,
              schema: e.target.value,
              table: '',
            };
            setLocalValue(temp);
            updateParent(temp);
          }}
          placeholder={`Select a ${
            tree[localValue.database]?.kind === 'bigquery'
              ? 'dataset'
              : 'schema'
          }...`}
          error={maybeError?.schema}
          data-testid={`${name}_schema`}
          icon={<FaFolder />}
          label={
            (tree[localValue.database]?.kind === 'bigquery'
              ? labels?.dataset
              : labels?.schema) ?? 'Schema'
          }
          name={`${name}_schema`}
        />
      </div>
      <div
        className={clsx(
          'col-span-12',
          hiddenKeys?.includes('table') ? 'hidden' : ''
        )}
      >
        <Select
          options={tableOptions}
          value={localValue.table}
          disabled={!tableOptions || disabledKeys?.includes('table')}
          onChange={e => {
            e.persist();
            const temp = {
              ...localValue,
              table: e.target.value,
            };
            setLocalValue(temp);
            updateParent(temp);
          }}
          placeholder="Select a table..."
          error={maybeError?.table}
          data-testid={`${name}_table`}
          icon={<FaTable />}
          label={labels?.table || 'Table'}
          name={`${name}_table`}
        />
      </div>
    </div>
  );
};
