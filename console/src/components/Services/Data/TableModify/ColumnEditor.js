import React from 'react';

import SearchableSelectBox from '../../../Common/SearchableSelect/SearchableSelect';
import CustomInputAutoSuggest from '../../../Common/CustomInputAutoSuggest/CustomInputAutoSuggest';

import { getValidAlterOptions } from './utils';

const ColumnEditor = ({
  onSubmit,
  dispatch,
  columnProperties,
  selectedProperties,
  editColumn,
  alterTypeOptions,
  defaultOptions,
}) => {
  const colName = columnProperties.name;

  if (!selectedProperties[colName]) {
    return null;
  }

  const styles = require('./ModifyTable.scss');

  const getColumnType = () => {
    return (
      colName in selectedProperties &&
      'type' in selectedProperties[colName] &&
      selectedProperties[colName].type
    );
  };
  const columnTypePG = getColumnType();

  const customSelectBoxStyles = {
    dropdownIndicator: {
      padding: '5px',
    },
    placeholder: {
      top: '44%',
      fontSize: '12px',
    },
    singleValue: {
      fontSize: '12px',
      top: '44%',
      color: '#555555',
    },
  };

  const { alterOptions, alterOptionsValueMap } = getValidAlterOptions(
    alterTypeOptions,
    colName
  );

  const updateColumnName = e => {
    dispatch(editColumn(colName, 'name', e.target.value));
  };
  const updateColumnType = selected => {
    dispatch(editColumn(colName, 'type', selected.value));
  };
  const updateColumnDef = (e, data) => {
    const { newValue } = data;
    dispatch(editColumn(colName, 'default', newValue));
  };
  const updateColumnComment = e => {
    dispatch(editColumn(colName, 'comment', e.target.value));
  };
  const toggleColumnNullable = e => {
    dispatch(editColumn(colName, 'isNullable', e.target.value === 'true'));
  };
  const toggleColumnUnique = e => {
    dispatch(editColumn(colName, 'isUnique', e.target.value === 'true'));
  };

  const getColumnDefaultInput = () => {
    const theme = require('../../../Common/CustomInputAutoSuggest/CustomThemes/EditColumnDefault.scss');

    return (
      <CustomInputAutoSuggest
        options={defaultOptions}
        className="input-sm form-control"
        value={selectedProperties[colName].default || ''}
        onChange={updateColumnDef}
        type="text"
        disabled={columnProperties.pkConstraint}
        data-test="edit-col-default"
        theme={theme}
      />
    );
  };

  return (
    <div className={`${styles.colEditor} container-fluid`}>
      <form className="form-horizontal" onSubmit={onSubmit}>
        <div className={`${styles.display_flex} form-group`}>
          <label className="col-xs-2">Name</label>
          <div className="col-xs-6">
            <input
              className="input-sm form-control"
              value={selectedProperties[colName].name}
              onChange={updateColumnName}
              type="text"
              data-test="edit-col-name"
            />
          </div>
        </div>
        <div className={`${styles.display_flex} form-group`}>
          <label className="col-xs-2">Type</label>
          <div className="col-xs-6">
            <SearchableSelectBox
              options={alterOptions}
              onChange={updateColumnType}
              value={columnTypePG && alterOptionsValueMap[columnTypePG]}
              bsClass={`col-type-${0} modify_select`}
              styleOverrides={customSelectBoxStyles}
              filterOption={'prefix'}
              placeholder="column_type"
            />
          </div>
        </div>
        <div className={`${styles.display_flex} form-group`}>
          <label className="col-xs-2">Nullable</label>
          <div className="col-xs-6">
            <select
              className="input-sm form-control"
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
        <div className={`${styles.display_flex} form-group`}>
          <label className="col-xs-2">Unique</label>
          <div className="col-xs-6">
            <select
              className="input-sm form-control"
              value={selectedProperties[colName].isUnique}
              onChange={toggleColumnUnique}
              disabled={columnProperties.pkConstraint}
              data-test="edit-col-unique"
            >
              <option value="true">True</option>
              <option value="false">False</option>
            </select>
          </div>
        </div>
        <div className={`${styles.display_flex} form-group`}>
          <label className="col-xs-2">Default</label>
          <div className="col-xs-6">{getColumnDefaultInput()}</div>
        </div>
        <div className={`${styles.display_flex} form-group`}>
          <label className="col-xs-2">Comment</label>
          <div className="col-xs-6">
            <input
              className="input-sm form-control"
              value={selectedProperties[colName].comment || ''}
              onChange={updateColumnComment}
              type="text"
              data-test="edit-col-comment"
            />
          </div>
        </div>
      </form>
      <div className="row">
        <br />
      </div>
    </div>
  );
};

export default ColumnEditor;
