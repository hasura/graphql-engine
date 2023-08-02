import React, { useEffect, useReducer, useState } from 'react';
import { connect, ConnectedProps } from 'react-redux';
import Select, { OnChangeValue } from 'react-select';

import { Button } from '../../../../new-components/Button';
import { Index, IndexType, Table } from '../../../../dataSources/types';
import { mapDispatchToPropsEmpty } from '../../../Common/utils/reactUtils';
import { removeIndex, saveIndex } from './ModifyActions';
import { showErrorNotification } from '../../Common/Notification';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import TextInput from '../../../Common/TextInput/TextInput';
import ToolTip from '../../../Common/Tooltip/Tooltip';
import { dataSource, isFeatureSupported } from '../../../../dataSources';
import { fetchTableIndexDetails } from '../DataActions';

type IndexState = {
  index_name: string;
  index_type: IndexType;
  index_columns: string[];
  unique?: boolean;
};

export const defaultIndexState: IndexState = {
  index_name: '',
  index_type: 'btree',
  index_columns: [],
  unique: false,
};

interface UpdateIndexName {
  type: 'Indexes/UPDATE_INDEX_NAME';
  data: string;
}

interface UpdateIndexUniqueState {
  type: 'Indexes/UPDATE_INDEX_UNIQUE_STATE';
  data: boolean;
}

interface UpdateIndexType {
  type: 'Indexes/UPDATE_INDEX_TYPE';
  data: IndexType;
}

interface UpdateIndexColumns {
  type: 'Indexes/UPDATE_INDEX_COLUMNS';
  data: string[];
}

interface ResetIndexState {
  type: 'Indexes/RESET_INDEX_STATE';
}

type IndexStateAction =
  | UpdateIndexColumns
  | UpdateIndexName
  | UpdateIndexType
  | UpdateIndexUniqueState
  | ResetIndexState;

const indexStateReducer = (
  state: IndexState,
  action: IndexStateAction
): IndexState => {
  switch (action.type) {
    case 'Indexes/UPDATE_INDEX_NAME':
      return {
        ...state,
        index_name: action.data,
      };
    case 'Indexes/UPDATE_INDEX_TYPE':
      return {
        ...state,
        index_type: action.data,
      };
    case 'Indexes/UPDATE_INDEX_COLUMNS':
      return {
        ...state,
        index_columns: action.data,
      };
    case 'Indexes/UPDATE_INDEX_UNIQUE_STATE':
      return {
        ...state,
        unique: action.data,
      };
    case 'Indexes/RESET_INDEX_STATE':
      return defaultIndexState;
    default:
      return state;
  }
};

const formTooltips = dataSource.indexFormToolTips;

const supportedIndex = dataSource.supportedIndex;

type IndexColumnsSelect = Record<'label' | 'value', string>;

interface IndexFieldsEditorProps extends ConnectorProps {
  currentTableInfo: Table;
}

const isUnique = (indexSql: string) => /CREATE\s+UNIQUE/i.test(indexSql);

const getDefCols = (indexSql: string) =>
  indexSql.split(/USING \w+ /)?.[1] ?? '';

interface CreateIndexProps {
  indexState: IndexState;
  tableColumnOptions: IndexColumnsSelect[];
  indexTypeOptions: Array<{
    label: string;
    value: IndexType;
  }>;
  onChangeIndextypeSelect: (
    value: OnChangeValue<IndexColumnsSelect, boolean>
  ) => void;
  updateIndexName: (name: string) => void;
  onChangeIndexColumnsSelect: (
    value: OnChangeValue<IndexColumnsSelect, boolean>
  ) => void;
  toggleIndexCheckboxState: (currentValue: boolean) => () => void;
}

const CreateIndexForm: React.FC<CreateIndexProps> = ({
  indexState,
  tableColumnOptions,
  indexTypeOptions,
  onChangeIndexColumnsSelect,
  updateIndexName,
  onChangeIndextypeSelect,
  toggleIndexCheckboxState,
}) => (
  <div>
    <div>
      <div className="mb-md">
        <div className="flex items-center mb-formlabel">
          <label htmlFor="index-name" className="text-gray-600 font-semibold">
            Index Name
          </label>
          {formTooltips?.indexName && (
            <ToolTip message={formTooltips.indexName} />
          )}
        </div>
        <TextInput
          onChange={e => updateIndexName(e.target.value)}
          value={indexState.index_name}
          id="index-name"
          placeholder="Input Name"
        />
      </div>
      <div className="mb-md">
        <div className="flex items-center mb-formlabel">
          <label
            className="text-gray-600 font-semibold"
            htmlFor="index-type-select"
          >
            Index Type
          </label>
          {formTooltips?.indexType && (
            <ToolTip message={formTooltips.indexType} />
          )}
        </div>
        <Select
          options={indexTypeOptions}
          className="legacy-input-fix"
          placeholder="-- select index type --"
          onChange={onChangeIndextypeSelect}
        />
      </div>
      <div className="mb-md">
        <div className="flex items-center mb-formlabel">
          <label
            className="text-gray-600 font-semibold"
            htmlFor="create-index-columns"
          >
            Columns
          </label>
          {formTooltips?.indexColumns && (
            <ToolTip message={formTooltips.indexColumns} />
          )}
        </div>
        <Select
          isMulti={supportedIndex?.multiColumn.includes(indexState.index_type)}
          options={tableColumnOptions}
          className="legacy-input-fix"
          placeholder="-- select columns --"
          onChange={onChangeIndexColumnsSelect}
        />
      </div>
      <div className="mb-md">
        <span className="flex items-center mb-formlabel">
          <input
            type="checkbox"
            id="index-unique"
            onChange={toggleIndexCheckboxState(indexState?.unique ?? false)}
            checked={indexState?.unique ?? false}
            style={{ margin: 0 }}
            className="legacy-input-fix"
          />
          <label className="ml-xs text-gray-600 mb-0" htmlFor="index-unique">
            Unique?
          </label>
          {formTooltips?.unique ? (
            <ToolTip message={formTooltips.unique} />
          ) : null}
        </span>
      </div>
    </div>
  </div>
);

const FieldsEditor: React.FC<IndexFieldsEditorProps> = props => {
  const { dispatch, currentTableInfo } = props;
  const [indexState, indexStateDispatch] = useReducer(
    indexStateReducer,
    defaultIndexState
  );
  const [indexes, setIndexes] = useState<Index[]>([]);

  const fetchIndexes = () => {
    dispatch(fetchTableIndexDetails(currentTableInfo)).then((data: Index[]) => {
      setIndexes(data);
    });
  };

  useEffect(fetchIndexes, []);

  const updateIndexName = (name: string) =>
    indexStateDispatch({ type: 'Indexes/UPDATE_INDEX_NAME', data: name });

  const toggleIndexCheckboxState = (currentValue: boolean) => () =>
    indexStateDispatch({
      type: 'Indexes/UPDATE_INDEX_UNIQUE_STATE',
      data: !currentValue,
    });

  const tableColumns = currentTableInfo.columns.map(
    column => column.column_name
  );
  const tableColumnOptions = tableColumns.reduce(
    (acc: IndexColumnsSelect[], columnName) => [
      ...acc,
      { label: columnName, value: columnName },
    ],
    []
  );

  const indexTypeOptions = Object.entries(dataSource.indexTypes ?? {}).map(
    ([key, value]) => ({
      label: key,
      value,
    })
  );

  const onChangeIndexColumnsSelect = (
    value: OnChangeValue<IndexColumnsSelect, boolean>
  ) => {
    if (value) {
      indexStateDispatch({
        type: 'Indexes/UPDATE_INDEX_COLUMNS',
        data: Array.isArray(value)
          ? value.map(val => val.value).slice(0, 32)
          : [(value as IndexColumnsSelect).value],
      });
    }
  };
  const onChangeIndextypeSelect = (
    value: OnChangeValue<IndexColumnsSelect, boolean>
  ) => {
    if (value) {
      indexStateDispatch({
        type: 'Indexes/UPDATE_INDEX_TYPE',
        data: (value as IndexColumnsSelect).value as IndexType,
      });
    }
  };

  const resetIndexEditState = () =>
    indexStateDispatch({ type: 'Indexes/RESET_INDEX_STATE' });

  const onSave = (toggleEditor: () => void) => {
    if (
      !indexState.index_name ||
      !indexState.index_columns?.length ||
      !indexState.index_type
    ) {
      dispatch(
        showErrorNotification(
          'Some Required Fields are Empty',
          'Index Name, Index Columns and Index Type are all required fields'
        )
      );
      return;
    }
    const successCb = () => {
      fetchIndexes();
      toggleEditor();
    };
    dispatch(saveIndex(indexState, successCb));
  };

  const onClickRemoveIndex = (indexInfo: Index) => () =>
    dispatch(removeIndex(indexInfo, fetchIndexes));

  const isPrimarykeyIndex = (a: Index) =>
    a.index_name === currentTableInfo.primary_key?.constraint_name;

  const pkSortFn = (a: Index, b: Index) =>
    Number(isPrimarykeyIndex(b)) - Number(isPrimarykeyIndex(a));

  const numberOfIndexes = indexes.length;

  // const indexCollapsedLabel = () => (
  //   <div className="italic text-sm text-gray-600">{numberOfIndexes ? '' : '(No primary key)'}</div>
  // );

  const editorExpanded = () => (
    <CreateIndexForm
      indexState={indexState}
      tableColumnOptions={tableColumnOptions}
      indexTypeOptions={indexTypeOptions}
      onChangeIndexColumnsSelect={onChangeIndexColumnsSelect}
      updateIndexName={updateIndexName}
      onChangeIndextypeSelect={onChangeIndextypeSelect}
      toggleIndexCheckboxState={toggleIndexCheckboxState}
    />
  );

  return (
    <>
      <div>
        {numberOfIndexes
          ? indexes.sort(pkSortFn).map(indexInfo => {
              const indexSql = indexInfo.index_definition_sql;
              return (
                <div key={indexInfo.index_name} className="mb-sm">
                  <Button
                    size="sm"
                    className="mr-sm"
                    disabled={isPrimarykeyIndex(indexInfo)}
                    onClick={onClickRemoveIndex(indexInfo)}
                  >
                    Remove
                  </Button>
                  <span className="font-semibold mr-sm">
                    {indexInfo.index_name}
                  </span>
                  <span>
                    {isPrimarykeyIndex(indexInfo) && (
                      <span className="mr-xs">PRIMARY KEY, </span>
                    )}
                    {isUnique(indexSql) && (
                      <span className="mr-xs">UNIQUE</span>
                    )}
                    <span className="uppercase mr-xs">
                      {indexInfo.index_type}
                    </span>
                    <b>
                      <span className="mr-xs">on</span>
                    </b>
                    <span>{getDefCols(indexSql)}</span>
                  </span>
                </div>
              );
            })
          : null}
      </div>
      {isFeatureSupported('tables.modify.indexes.edit') ? (
        <ExpandableEditor
          editorExpanded={editorExpanded}
          property="create-index"
          service="modify-table"
          saveFunc={onSave}
          expandButtonText="Add a index"
          collapseButtonText="Cancel"
          collapseCallback={resetIndexEditState}
        />
      ) : null}
    </>
  );
};

const indexFieldsEditorConnector = connect(null, mapDispatchToPropsEmpty);
type ConnectorProps = ConnectedProps<typeof indexFieldsEditorConnector>;
const IndexFieldsEditor = indexFieldsEditorConnector(FieldsEditor);

export default IndexFieldsEditor;
