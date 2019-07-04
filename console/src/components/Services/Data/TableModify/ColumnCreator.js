import React, { useState } from 'react';
import { showErrorNotification } from '../../Common/Notification';
import gqlPattern, { gqlColumnErrorNotif } from '../Common/GraphQLValidation';
import { commonDataTypes } from '../utils';

import SearchableSelectBox from '../../../Common/SearchableSelect/SearchableSelect';
import CustomInputAutoSuggest from '../../../Common/CustomInputAutoSuggest/CustomInputAutoSuggest';

import {
  getDataOptions,
  getDefaultFunctionsOptions,
  inferDefaultValues,
} from '../Common/utils';

import Button from '../../../Common/Button/Button';
import { addColSql } from '../TableModify/ModifyActions';

import styles from './ModifyTable.scss';

const useColumnEditor = (dispatch, tableName) => {
  const initialState = {
    colName: '',
    colType: '',
    colNull: true,
    colUnique: false,
    colDefault: '',
  };

  const [columnState, setColumnState] = useState(initialState);
  const { colName, colType, colNull, colUnique, colDefault } = columnState;

  const onSubmit = e => {
    e.preventDefault();

    // validate before sending
    if (!gqlPattern.test(colName)) {
      dispatch(
        showErrorNotification(
          gqlColumnErrorNotif[0],
          gqlColumnErrorNotif[1],
          gqlColumnErrorNotif[3]
        )
      );
    } else if (colName === '' || colType === '') {
      dispatch(
        showErrorNotification(
          'Error creating column!',
          'Column name/type cannot be empty',
          {
            custom: 'Column name/type cannot be empty',
          }
        )
      );
    } else {
      dispatch(
        addColSql(
          tableName,
          colName,
          colType,
          colNull,
          colUnique,
          colDefault,
          () => setColumnState(initialState)
        )
      );
    }
  };

  return {
    colName: {
      value: colName,
      onChange: e => {
        setColumnState({ ...columnState, colName: e.target.value });
      },
    },
    colType: {
      value: colType,
      onChange: selected => {
        setColumnState({ ...columnState, colType: selected.value });
      },
    },
    colNull: {
      checked: colNull,
      onChange: e => {
        setColumnState({ ...columnState, colNull: e.target.checked });
      },
    },
    colUnique: {
      checked: colUnique,
      onChange: e => {
        setColumnState({ ...columnState, colUnique: e.target.checked });
      },
    },
    colDefault: {
      value: colDefault,
      onChange: (e, data) => {
        const { newValue } = data;
        setColumnState({ ...columnState, colDefault: newValue });
      },
    },
    onSubmit,
  };
};

const ColumnCreator = ({
  dispatch,
  tableName,
  dataTypes: restTypes = [],
  validTypeCasts,
  columnDefaultFunctions,
}) => {
  const {
    colName,
    colType,
    colNull,
    colUnique,
    colDefault,
    onSubmit,
  } = useColumnEditor(dispatch, tableName);

  let defaultOptions = [];

  const getInferredDefaultValues = () =>
    inferDefaultValues(columnDefaultFunctions, validTypeCasts)(colType.value);

  const colDefaultFunctions =
    colType.value in columnDefaultFunctions
      ? columnDefaultFunctions[colType.value]
      : getInferredDefaultValues();

  if (colDefaultFunctions && colDefaultFunctions.length > 0) {
    defaultOptions = getDefaultFunctionsOptions(colDefaultFunctions, 0);
  }

  const getDefaultInput = () => {
    const theme = require('../../../Common/CustomInputAutoSuggest/CustomThemes/AddColumnDefault.scss');

    return (
      <CustomInputAutoSuggest
        placeholder="default value"
        options={defaultOptions}
        className={`${styles.input}
          ${styles.defaultInput}
          input-sm form-control`}
        {...colDefault}
        data-test="default-value"
        theme={theme}
      />
    );
  };

  const { columnDataTypes, columnTypeValueMap } = getDataOptions(
    commonDataTypes,
    restTypes,
    0
  );

  const customSelectBoxStyles = {
    container: {
      width: '186px',
    },
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

  return (
    <div className={styles.activeEdit}>
      <form
        className={`form-inline ${styles.display_flex}`}
        onSubmit={onSubmit}
      >
        <input
          placeholder="column name"
          type="text"
          className={`${styles.input} input-sm form-control`}
          data-test="column-name"
          {...colName}
        />
        <span className={`${styles.select}`} data-test="col-type-0">
          <SearchableSelectBox
            options={columnDataTypes}
            onChange={colType.onChange}
            value={colType.value && columnTypeValueMap[colType.value]}
            bsClass={`col-type-${0} modify_select`}
            styleOverrides={customSelectBoxStyles}
            filterOption={'prefix'}
            placeholder="column_type"
          />
        </span>
        <input
          type="checkbox"
          className={`${styles.input} ${styles.nullable} input-sm form-control`}
          data-test="nullable-checkbox"
          {...colNull}
        />
        <label className={styles.nullLabel}>Nullable</label>

        <input
          type="checkbox"
          className={`${styles.input} ${styles.nullable} input-sm form-control`}
          {...colUnique}
          data-test="unique-checkbox"
        />
        <label className={styles.nullLabel}>Unique</label>
        {getDefaultInput()}
        {/*
        <input
          placeholder="default value"
          type="text"
          className={`${styles.input} ${
            styles.defaultInput
          } input-sm form-control`}
          {...colDefault}
          data-test="default-value"
        />
        */}

        <Button
          type="submit"
          color="yellow"
          size="sm"
          data-test="add-column-button"
        >
          + Add column
        </Button>
      </form>
    </div>
  );
};

export default ColumnCreator;
