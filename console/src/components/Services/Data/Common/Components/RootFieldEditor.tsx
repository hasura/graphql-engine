import React from 'react';
import CollapsibleToggle from '../../../../Common/CollapsibleToggle/CollapsibleToggle';

import { getRootFieldLabel } from './utils';

interface RootFieldEditorProps {
  rootFields: Record<string, string>;
  disabled: boolean;
  tableName: string;
  tableSchema: string;
  customName?: string;
  customNameOnChange: ChangeHandler;
  selectOnChange: ChangeHandler;
  selectByPkOnChange: ChangeHandler;
  selectAggOnChange: ChangeHandler;
  insertOnChange: ChangeHandler;
  insertOneOnChange: ChangeHandler;
  updateOnChange: ChangeHandler;
  updateByPkOnChange: ChangeHandler;
  deleteOnChange: ChangeHandler;
  deleteByPkOnChange: ChangeHandler;
}

type ChangeHandler = (e: React.ChangeEvent<HTMLInputElement>) => void;

const RootFieldEditor: React.FC<RootFieldEditorProps> = ({
  rootFields,
  disabled,
  customNameOnChange,
  selectOnChange,
  selectByPkOnChange,
  selectAggOnChange,
  insertOnChange,
  insertOneOnChange,
  updateOnChange,
  updateByPkOnChange,
  deleteOnChange,
  deleteByPkOnChange,
  tableName,
  customName,
  tableSchema,
}) => {
  const {
    select,
    select_by_pk: selectByPk,
    select_aggregate: selectAgg,
    insert,
    insert_one: insertOne,
    update,
    update_by_pk: updateByPk,
    delete: _delete,
    delete_by_pk: deleteByPk,
  } = rootFields;

  const getRootField = () => {
    if (customName) {
      return customName;
    }

    if (tableSchema) {
      return `${tableSchema}_${tableName}`;
    }

    return tableName;
  };

  const getDefaultRootField = (rfType: string) => {
    const rootField = getRootField();

    if (rfType.includes('select')) {
      return rfType.replace('select', rootField);
    }
    if (rfType.includes('one')) {
      return `${rfType.replace('one', rootField)}_one`;
    }
    if (rfType === 'custom_name') {
      return rootField;
    }
    return `${rfType}_${rootField}`;
  };

  const getRow = (
    rfType: string,
    value: string | undefined,
    onChange: ChangeHandler
  ) => (
    <div className="flex items-center">
      <div className="text-gray-600">{getRootFieldLabel(rfType)}</div>
      <div className="ml-auto w-6/12">
        <input
          type="text"
          value={value || ''}
          placeholder={`${getDefaultRootField(rfType)} (default)`}
          className="form-control"
          onChange={onChange}
          disabled={disabled}
        />
      </div>
    </div>
  );

  const getSection = (rfType: string) => {
    return (
      <div>
        <CollapsibleToggle
          title={rfType === 'query' ? 'Query and Subscription' : 'Mutation'}
          useDefaultTitleStyle
          isOpen
        >
          {rfType === 'query' && (
            <div className="space-y-md mb-md">
              {getRow('select', select, selectOnChange)}
              {getRow('select_by_pk', selectByPk, selectByPkOnChange)}
              {getRow('select_aggregate', selectAgg, selectAggOnChange)}
            </div>
          )}
          {rfType === 'mutation' && (
            <div className="space-y-md mb-md">
              {getRow('insert', insert, insertOnChange)}
              {getRow('insert_one', insertOne, insertOneOnChange)}
              {getRow('update', update, updateOnChange)}
              {getRow('update_by_pk', updateByPk, updateByPkOnChange)}
              {getRow('delete', _delete, deleteOnChange)}
              {getRow('delete_by_pk', deleteByPk, deleteByPkOnChange)}
            </div>
          )}
        </CollapsibleToggle>
      </div>
    );
  };

  return (
    <div>
      <div>
        <div className="mb-md">
          {getRow('custom_name', customName, customNameOnChange)}
        </div>
        {getSection('query')}
        {getSection('mutation')}
      </div>
    </div>
  );
};

export default RootFieldEditor;
