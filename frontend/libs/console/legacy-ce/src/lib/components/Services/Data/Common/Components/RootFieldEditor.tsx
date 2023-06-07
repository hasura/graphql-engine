import React from 'react';
import { CustomRootFields } from '../../../../../dataSources/types';
import CollapsibleToggle from '../../../../Common/CollapsibleToggle/CollapsibleToggle';

import { Nullable } from '../../../../../components/Common/utils/tsUtils';
import {
  getTableCustomRootFieldComment,
  getTableCustomRootFieldName,
} from '../../../../../dataSources';
import { CommentInput } from './CommentInput';

interface RootFieldEditorProps {
  rootFields: CustomRootFields;
  disabled: boolean;
  tableName: string;
  tableSchema: string;
  customName?: string;
  customNameOnChange: (e: React.ChangeEvent<HTMLInputElement>) => void;
  selectOnChange: ChangeHandler;
  selectByPkOnChange: ChangeHandler;
  selectAggOnChange: ChangeHandler;
  selectStreamOnChange: ChangeHandler;
  insertOnChange: ChangeHandler;
  insertOneOnChange: ChangeHandler;
  updateOnChange: ChangeHandler;
  updateByPkOnChange: ChangeHandler;
  deleteOnChange: ChangeHandler;
  deleteByPkOnChange: ChangeHandler;
  updateManyOnChange: ChangeHandler;
}

interface ChangeHandler {
  onNameChange: (e: React.ChangeEvent<HTMLInputElement>) => void;
  onCommentChange: (comment: string | null) => void;
}

export const rootFieldLabels: Record<keyof CustomRootFields, string> = {
  select: 'Select',
  select_by_pk: 'Select by PK',
  select_aggregate: 'Select Aggregate',
  select_stream: 'Select Stream',
  insert: 'Insert',
  insert_one: 'Insert One',
  update: 'Update',
  update_by_pk: 'Update by PK',
  delete: 'Delete',
  delete_by_pk: 'Delete by PK',
  update_many: 'Update many',
};

const RootFieldEditor: React.FC<RootFieldEditorProps> = ({
  rootFields,
  disabled,
  customNameOnChange,
  selectOnChange,
  selectByPkOnChange,
  selectAggOnChange,
  selectStreamOnChange,
  insertOnChange,
  insertOneOnChange,
  updateOnChange,
  updateByPkOnChange,
  deleteOnChange,
  deleteByPkOnChange,
  updateManyOnChange,
  tableName,
  customName,
  tableSchema,
}) => {
  const qualifiedTableName =
    tableSchema === '' ? `"${tableName}"` : `"${tableSchema}.${tableName}"`;
  const rootFieldDefaultComments: Record<keyof CustomRootFields, string> = {
    select: `fetch data from the table: ${qualifiedTableName}`,
    select_by_pk: `fetch data from the table: ${qualifiedTableName} using primary key columns`,
    select_aggregate: `fetch aggregated fields from the table: ${qualifiedTableName}`,
    select_stream: `fetch stream fields from the table: ${qualifiedTableName}`,
    insert: `insert data into the table: ${qualifiedTableName}`,
    insert_one: `insert a single row into the table: ${qualifiedTableName}`,
    update: `update data of the table: ${qualifiedTableName}`,
    update_by_pk: `update single row of the table: ${qualifiedTableName}`,
    delete: `delete data from the table: ${qualifiedTableName}`,
    delete_by_pk: `delete single row from the table: ${qualifiedTableName}`,
    update_many: `update data for "many" operations of the table: ${qualifiedTableName}`,
  };

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
    if (rfType.includes('_by_pk')) {
      const [type] = rfType.split('_by_pk');
      return `${type}_${rootField}_by_pk`;
    }
    if (rfType === 'custom_name') {
      return rootField;
    }
    return `${rfType}_${rootField}`;
  };

  const getCustomNameRow = (
    value: Nullable<string>,
    onChange: (e: React.ChangeEvent<HTMLInputElement>) => void
  ) => (
    <div className="flex items-center space-x-4 ">
      <div className="text-gray-600 w-2/12">Custom Table Name</div>
      <div className="w-4/12">
        <input
          type="text"
          value={value || ''}
          placeholder={`${getDefaultRootField('custom_name')} (default)`}
          className="form-control"
          onChange={onChange}
          disabled={disabled}
        />
      </div>
      <div className="w-6/12" />
    </div>
  );

  const getRootFieldRow = (
    rfType: keyof CustomRootFields,
    onChange: ChangeHandler
  ) => (
    <div className="flex items-center space-x-4">
      <div className="text-gray-600 w-2/12">{rootFieldLabels[rfType]}</div>
      <div className="w-4/12">
        <input
          type="text"
          value={getTableCustomRootFieldName(rootFields[rfType]) || ''}
          placeholder={`${getDefaultRootField(rfType)} (default)`}
          className="form-control"
          onChange={onChange.onNameChange}
          disabled={disabled}
        />
      </div>
      <div className="w-6/12">
        <CommentInput
          value={getTableCustomRootFieldComment(rootFields[rfType])}
          defaultComment={rootFieldDefaultComments[rfType]}
          onChange={onChange.onCommentChange}
        />
      </div>
    </div>
  );

  const getSection = (rfType: 'query' | 'mutation') => {
    return (
      <div>
        <CollapsibleToggle
          title={rfType === 'query' ? 'Query and Subscription' : 'Mutation'}
          useDefaultTitleStyle
          isOpen
        >
          <div className="flex items-center space-x-4 pb-2">
            <div className="text-gray-600 w-2/12" />
            <div className="text-gray-600 w-4/12">Field Name</div>
            <div className="text-gray-600 w-6/12">Comment </div>
          </div>
          {rfType === 'query' && (
            <div className="space-y-md mb-md">
              {getRootFieldRow('select', selectOnChange)}
              {getRootFieldRow('select_by_pk', selectByPkOnChange)}
              {getRootFieldRow('select_aggregate', selectAggOnChange)}
              {getRootFieldRow('select_stream', selectStreamOnChange)}
            </div>
          )}
          {rfType === 'mutation' && (
            <div className="space-y-md mb-md">
              {getRootFieldRow('insert', insertOnChange)}
              {getRootFieldRow('insert_one', insertOneOnChange)}
              {getRootFieldRow('update', updateOnChange)}
              {getRootFieldRow('update_by_pk', updateByPkOnChange)}
              {getRootFieldRow('update_many', updateManyOnChange)}
              {getRootFieldRow('delete', deleteOnChange)}
              {getRootFieldRow('delete_by_pk', deleteByPkOnChange)}
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
          {getCustomNameRow(customName, customNameOnChange)}
        </div>
        {getSection('query')}
        {getSection('mutation')}
      </div>
    </div>
  );
};

export default RootFieldEditor;
