import React, { useState } from 'react';
import { Dispatch } from '../../../../types';
import {
  DependentSQLGenerator,
  FrequentlyUsedColumn,
} from '../../../../dataSources/types';
import { addColSql } from './ModifyActions';
import { showErrorNotification } from '../../Common/Notification';
import gqlPattern, { gqlColumnErrorNotif } from '../Common/GraphQLValidation';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import CustomInputAutoSuggest from '../../../Common/CustomInputAutoSuggest/CustomInputAutoSuggest';
import type { AutoSuggestSection } from '../../../Common/CustomInputAutoSuggest/CustomInputAutoSuggest';
import {
  getDataOptions,
  getDefaultFunctionsOptions,
  inferDefaultValues,
} from '../Common/utils';
import FrequentlyUsedColumnSelector from '../Common/Components/FrequentlyUsedColumnSelector';
import { ColumnTypeSelector } from '../Common/Components/ColumnTypeSelector';
import { dataSource, isFeatureSupported } from '../../../../dataSources';
import { focusYellowRing, inputStyles } from '../constants';

const initialColumnEditorState = {
  colName: '',
  colType: '',
  colNull: true,
  colUnique: false,
  colDefault: '',
  colDependentSQLGenerator: undefined,
};

const useColumnEditor = (dispatch: Dispatch, tableName: string) => {
  const [columnState, setColumnState] = useState<{
    colName: string;
    colType: string;
    colNull: boolean;
    colUnique: boolean;
    colDefault: string;
    colDependentSQLGenerator?: DependentSQLGenerator;
  }>(initialColumnEditorState);
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
          () => setColumnState(initialColumnEditorState)
        )
      );
    }
  };

  return {
    colName: {
      value: colName,
      onChange: (value: string) => {
        setColumnState({ ...columnState, colName: value });
      },
    },
    colType: {
      value: colType,
      onChange: (value: string) => {
        setColumnState({ ...columnState, colType: value });
      },
    },
    colNull: {
      checked: colNull,
      onChange: (value: boolean) => {
        setColumnState({ ...columnState, colNull: value });
      },
    },
    colUnique: {
      checked: colUnique,
      onChange: (value: boolean) => {
        setColumnState({ ...columnState, colUnique: value });
      },
    },
    colDefault: {
      value: colDefault,
      onChange: (value: string) => {
        setColumnState({ ...columnState, colDefault: value });
      },
    },
    frequentlyUsedColumn: {
      onSelect: (column: FrequentlyUsedColumn) => {
        setColumnState({
          ...initialColumnEditorState,
          colName: column.name,
          colType: column.type,
          colDefault: column.default ?? '',
          colDependentSQLGenerator: column.dependentSQLGenerator,
        });
      },
    },
    onSubmit,
  };
};

interface ColumnCreatorProps {
  dispatch: Dispatch;
  tableName: string;
  dataTypes: string[][];
  validTypeCasts: Record<string, string[]>;
  columnDefaultFunctions: Record<string, string[]>;
  postgresVersion: string;
}

const ColumnCreator: React.VFC<ColumnCreatorProps> = ({
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
        value={colName.value}
        onChange={e => colName.onChange(e.target.value)}
        placeholder="column name"
        type="text"
        className={inputStyles}
        data-test="column-name"
      />
    );
  };

  const getColumnTypeInput = () => {
    const { columnDataTypes, columnTypeValueMap } = getDataOptions(
      dataSource.commonDataTypes,
      restTypes,
      0
    );

    return (
      <span data-test="col-type-0">
        {isFeatureSupported('tables.create.frequentlyUsedColumns') ? (
          <ColumnTypeSelector
            options={columnDataTypes}
            onChange={option => colType.onChange(option.value)}
            value={(columnTypeValueMap as any)[colType.value] || colType.value}
            colIdentifier={0}
            bsClass="col-type-0"
          />
        ) : (
          <input
            type="text"
            className={inputStyles}
            onChange={e => {
              e.persist();
              colType.onChange(e.target.value);
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
        <label className="flex items-center mr-sm">
          <input
            type="checkbox"
            checked={colNull.checked}
            onChange={e => colNull.onChange(e.target.checked)}
            style={{ margin: '0' }}
            className={focusYellowRing}
            data-test="nullable-checkbox"
          />
          <span className="ml-xs">Nullable</span>
        </label>
      </span>
    );
  };

  const getColumnUniqueInput = () => {
    return (
      <span>
        <label className="flex items-center mr-sm">
          <input
            type="checkbox"
            checked={colUnique.checked}
            onChange={e => colUnique.onChange(e.target.checked)}
            style={{ margin: '0' }}
            className={focusYellowRing}
            data-test="unique-checkbox"
          />
          <span className="ml-xs">Unique</span>
        </label>
      </span>
    );
  };

  const getColumnDefaultInput = () => {
    const getInferredDefaultValues = () =>
      inferDefaultValues(columnDefaultFunctions, validTypeCasts)(colType.value);

    const colDefaultFunctions =
      colType.value in columnDefaultFunctions
        ? columnDefaultFunctions[colType.value]
        : getInferredDefaultValues();

    let defaultOptions: AutoSuggestSection[] = [];
    if (colDefaultFunctions && colDefaultFunctions.length > 0) {
      defaultOptions = getDefaultFunctionsOptions(colDefaultFunctions, 0);
    }

    return (
      <CustomInputAutoSuggest
        placeholder="default value"
        options={defaultOptions}
        className={inputStyles}
        value={colDefault.value}
        onChange={(_, params) => colDefault.onChange(params.newValue)}
        data-test="default-value"
      />
    );
  };

  const getFrequentlyUsedColumnSelector = () => {
    return (
      <FrequentlyUsedColumnSelector
        onSelect={frequentlyUsedColumn.onSelect}
        action="modify"
        postgresVersion={postgresVersion}
      />
    );
  };

  const expandedContent = () => (
    <div>
      <form className="mb-sm grid gap-sm grid-cols-1 sm:grid-cols-4">
        {getColumnNameInput()}
        {getColumnTypeInput()}
        {getColumnDefaultInput()}
        <div className="flex items-center">
          {getColumnNullableInput()}
          {getColumnUniqueInput()}
        </div>
      </form>
      <div>
        {isFeatureSupported('tables.modify.columns.frequentlyUsedColumns')
          ? getFrequentlyUsedColumnSelector()
          : null}
      </div>
    </div>
  );

  return (
    <ExpandableEditor
      key="new-col"
      editorExpanded={expandedContent}
      property="add-new-column"
      service="modify-table"
      expandButtonText="Add a new column"
      saveFunc={onSubmit}
      isCollapsable
    />
  );
};

export default ColumnCreator;
