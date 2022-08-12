import React from 'react';

import CustomInputAutoSuggest from '../../../Common/CustomInputAutoSuggest/CustomInputAutoSuggest';

import { getValidAlterOptions, convertToArrayOptions } from './utils';
import Tooltip from '../../../Common/Tooltip/Tooltip';
import { ColumnTypeSelector } from '../Common/Components/ColumnTypeSelector';
import { dataSource, isFeatureSupported } from '../../../../dataSources';
import { inputStyles } from '../constants';

const ColumnEditor = ({
  onSubmit,
  dispatch,
  columnProperties,
  selectedProperties,
  editColumn,
  alterTypeOptions,
  defaultOptions,
}) => {
  const { name: colName, isArrayDataType } = columnProperties;

  if (!selectedProperties[colName]) {
    return null;
  }

  const getColumnType = () => {
    return (
      colName in selectedProperties &&
      'type' in selectedProperties[colName] &&
      selectedProperties[colName].type
    );
  };
  // todo — data-sources
  let columnTypePG = getColumnType();
  if (columnProperties.display_type_name === dataSource.columnDataTypes.ARRAY) {
    columnTypePG = columnTypePG.replace('_', '') + '[]';
  }

  // eslint-disable-next-line prefer-const
  let { alterOptions, alterOptionsValueMap } = getValidAlterOptions(
    alterTypeOptions,
    colName
  );

  // todo — data-sources
  if (isArrayDataType) {
    alterOptions = convertToArrayOptions(alterOptions);
  }

  const updateColumnName = e => {
    dispatch(editColumn(colName, 'name', e.target.value));
  };
  const updateColumnType = selected => {
    dispatch(editColumn(colName, 'type', selected.value));
  };
  const toggleColumnNullable = e => {
    dispatch(editColumn(colName, 'isNullable', e.target.value === 'true'));
  };
  const toggleColumnUnique = e => {
    dispatch(editColumn(colName, 'isUnique', e.target.value === 'true'));
  };
  const updateColumnDefault = (e, data) => {
    const { newValue } = data;
    dispatch(editColumn(colName, 'default', newValue));
  };
  const updateColumnComment = e => {
    dispatch(editColumn(colName, 'comment', e.target.value));
  };
  const updateColumnCustomField = e => {
    dispatch(editColumn(colName, 'customFieldName', e.target.value));
  };

  const getColumnCustomFieldInput = () => {
    return (
      <div className="flex items-center">
        <label className="flex items-center text-gray-600 font-semibold">
          GraphQL Field Name
          <Tooltip
            message={
              'Expose the column with a different name in the GraphQL API'
            }
          />
        </label>
        <div className="ml-auto w-6/12">
          <input
            className={`${inputStyles} disabled:bg-gray-100`}
            value={selectedProperties[colName].customFieldName}
            onChange={updateColumnCustomField}
            placeholder={`${colName} (default)`}
            type="text"
            data-test="edit-col-custom-field"
          />
        </div>
      </div>
    );
  };

  const getColumnDefaultInput = () => {
    return (
      <CustomInputAutoSuggest
        options={defaultOptions}
        className={inputStyles}
        value={selectedProperties[colName].default || ''}
        onChange={updateColumnDefault}
        type="text"
        data-test="edit-col-default"
      />
    );
  };

  return (
    <div>
      <form className="space-y-md" onSubmit={onSubmit}>
        <div className="flex items-center">
          <label className="flex items-center text-gray-600 font-semibold">
            Name
          </label>
          <div className="ml-auto w-6/12">
            <input
              className={`${inputStyles} disabled:bg-gray-100 w-full`}
              value={selectedProperties[colName].name}
              onChange={updateColumnName}
              type="text"
              data-test="edit-col-name"
            />
          </div>
        </div>
        <div className="flex items-center">
          <label className="flex items-center text-gray-600 font-semibold">
            Type
          </label>
          <div className="ml-auto w-6/12">
            {isFeatureSupported('tables.create.frequentlyUsedColumns') ? (
              <ColumnTypeSelector
                options={alterOptions}
                onChange={updateColumnType}
                value={alterOptionsValueMap[columnTypePG] || columnTypePG}
                colIdentifier={0}
                bsClass={`col-type-${0}`}
              />
            ) : (
              <input
                type="text"
                className={`${inputStyles} disabled:bg-gray-100`}
                value={
                  alterOptionsValueMap?.[columnTypePG]?.value ?? columnTypePG
                }
                onChange={e => {
                  e.persist();
                  updateColumnType({ value: e.target.value });
                }}
                placeholder="column_type"
              />
            )}
          </div>
        </div>
        <div className="flex items-center">
          <label className="flex items-center text-gray-600 font-semibold">
            Nullable
          </label>
          <div className="ml-auto w-6/12">
            <select
              className={`${inputStyles} disabled:bg-gray-100`}
              value={selectedProperties[colName].isNullable}
              onChange={toggleColumnNullable}
              disabled={columnProperties.pkConstraint}
              data-test="edit-col-nullable"
            >
              <option value="true">True</option>
              <option value="false">False</option>
            </select>
          </div>
        </div>
        <div className="flex items-center">
          <label className="flex items-center text-gray-600 font-semibold">
            Unique
          </label>
          <div className="ml-auto w-6/12">
            <select
              className={`${inputStyles} disabled:bg-gray-100`}
              value={
                !!(
                  selectedProperties[colName].isUnique ||
                  columnProperties.isOnlyPrimaryKey
                )
              }
              onChange={toggleColumnUnique}
              disabled={columnProperties.pkConstraint}
              data-test="edit-col-unique"
            >
              <option value="true">True</option>
              <option value="false">False</option>
            </select>
          </div>
        </div>
        <div className="flex items-center">
          <label className="flex items-center text-gray-600 font-semibold">
            Default
          </label>
          <div className="ml-auto w-6/12">{getColumnDefaultInput()}</div>
        </div>
        <div className="flex items-center">
          <label className="flex items-center text-gray-600 font-semibold">
            Comment
          </label>
          <div className="ml-auto w-6/12">
            <input
              className={`${inputStyles} disabled:bg-gray-100`}
              value={selectedProperties[colName].comment || ''}
              onChange={updateColumnComment}
              type="text"
              data-test="edit-col-comment"
            />
          </div>
        </div>
        {isFeatureSupported('tables.modify.columns.graphqlFieldName')
          ? getColumnCustomFieldInput()
          : null}
      </form>
    </div>
  );
};

export default ColumnEditor;
