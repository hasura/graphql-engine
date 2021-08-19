import React, { useState } from 'react';
import { showErrorNotification } from '../../Common/Notification';
import gqlPattern, { gqlColumnErrorNotif } from '../Common/GraphQLValidation';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import CustomInputAutoSuggest from '../../../Common/CustomInputAutoSuggest/CustomInputAutoSuggest';

import {
  getDataOptions,
  getDefaultFunctionsOptions,
  inferDefaultValues,
} from '../Common/utils';

import { addColSql } from '../TableModify/ModifyActions';

import styles from './ModifyTable.scss';
import FrequentlyUsedColumnSelector from '../Common/Components/FrequentlyUsedColumnSelector';
import { ColumnTypeSelector } from '../Common/Components/ColumnTypeSelector';
import { dataSource, isFeatureSupported } from '../../../../dataSources';

const useColumnEditor = (dispatch, tableName) => {
  const initialState = {
    colName: '',
    colType: '',
    colNull: true,
    colUnique: false,
    colDefault: '',
    colDependentSQLGenerator: null,
  };

  const [columnState, setColumnState] = useState(initialState);
  const {
    colName,
    colType,
    colNull,
    colUnique,
    colDefault,
    colDependentSQLGenerator,
  } = columnState;

  const onSubmit = () => {
    // auto-trim column name
    const trimmedColName = colName.trim();

    // validate before sending
    if (!gqlPattern.test(trimmedColName)) {
      dispatch(
        showErrorNotification(
          gqlColumnErrorNotif[0],
          gqlColumnErrorNotif[1],
          gqlColumnErrorNotif[2]
        )
      );
    } else if (trimmedColName === '' || colType === '') {
      dispatch(
        showErrorNotification(
          'Error creating column!',
          'Column name/type cannot be empty'
        )
      );
    } else {
      dispatch(
        addColSql(
          tableName,
          trimmedColName,
          colType,
          colNull,
          colUnique,
          colDefault,
          colDependentSQLGenerator,
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
    frequentlyUsedColumn: {
      onSelect: fuc => {
        setColumnState({
          ...initialState,
          colName: fuc.name,
          colType: fuc.type,
          colDefault: fuc.default,
          colDependentSQLGenerator: fuc.dependentSQLGenerator,
        });
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
  postgresVersion,
}) => {
  const {
    colName,
    colType,
    colNull,
    colUnique,
    colDefault,
    frequentlyUsedColumn,
    onSubmit,
  } = useColumnEditor(dispatch, tableName);

  const getColumnNameInput = () => {
    return (
      <input
        placeholder="column name"
        type="text"
        className={`${styles.input} input-sm form-control`}
        data-test="column-name"
        {...colName}
      />
    );
  };

  const getColumnTypeInput = () => {
    const { columnDataTypes, columnTypeValueMap } = getDataOptions(
      dataSource.commonDataTypes,
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
      <span className={styles.select} data-test="col-type-0">
        {isFeatureSupported('tables.create.frequentlyUsedColumns') ? (
          <ColumnTypeSelector
            options={columnDataTypes}
            onChange={colType.onChange}
            value={columnTypeValueMap[colType.value] || colType.value}
            colIdentifier={0}
            bsClass={`col-type-${0} modify_select`}
            styleOverrides={customSelectBoxStyles}
          />
        ) : (
          <input
            type="text"
            className={`${styles.input} form-control col-type-${0} ${
              styles.max_width
            }`}
            onChange={e => {
              e.persist();
              colType.onChange({ value: e.target.value });
            }}
            placeholder="column_type"
          />
        )}
      </span>
    );
  };

  const getColumnNullableInput = () => {
    return (
      <span>
        <label className={styles.nullLabel}>
          <input
            type="checkbox"
            className={`${styles.input} ${styles.nullable} input-sm form-control legacy-input-fix`}
            data-test="nullable-checkbox"
            {...colNull}
          />
          Nullable
        </label>
      </span>
    );
  };

  const getColumnUniqueInput = () => {
    return (
      <span>
        <label className={styles.nullLabel}>
          <input
            type="checkbox"
            className={`${styles.input} ${styles.nullable} input-sm form-control legacy-input-fix`}
            {...colUnique}
            data-test="unique-checkbox"
          />
          Unique
        </label>
      </span>
    );
  };

  const getColumnDefaultInput = () => {
    const theme = require('../../../Common/CustomInputAutoSuggest/CustomThemes/AddColumnDefault.scss');

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

  const getFrequentlyUsedColumnSelector = () => {
    return (
      <FrequentlyUsedColumnSelector
        onSelect={frequentlyUsedColumn.onSelect}
        action={'modify'}
        postgresVersion={postgresVersion}
      />
    );
  };

  const expandedContent = () => (
    <div>
      <form className={`form-inline ${styles.display_flex}`}>
        {getColumnNameInput()}
        {getColumnTypeInput()}
        {getColumnNullableInput()}
        {getColumnUniqueInput()}
        {getColumnDefaultInput()}
      </form>
      <div className={styles.add_mar_top_small}>
        {isFeatureSupported('tables.modify.columns.frequentlyUsedColumns')
          ? getFrequentlyUsedColumnSelector()
          : null}
      </div>
    </div>
  );

  return (
    <ExpandableEditor
      key={'new-col'}
      editorExpanded={expandedContent}
      property={'add-new-column'}
      service={'modify-table'}
      expandButtonText={'Add a new column'}
      saveFunc={onSubmit}
      isCollapsable
    />
  );
};

export default ColumnCreator;
