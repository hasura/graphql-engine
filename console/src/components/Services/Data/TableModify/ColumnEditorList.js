import React from 'react';
import ColumnEditor from './ColumnEditor';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import {
  saveColumnChangesSql,
  deleteColumnSql,
  DELETE_PK_WARNING,
  setColumnEdit,
  resetColumnEdit,
  editColumn,
  isColumnUnique,
} from '../TableModify/ModifyActions';
import { ordinalColSort } from '../utils';
import { defaultDataTypeToCast } from '../constants';

import {
  getDefaultFunctionsOptions,
  inferDefaultValues,
} from '../Common/utils';

import GqlCompatibilityWarning from '../../../Common/GqlCompatibilityWarning/GqlCompatibilityWarning';

import styles from './ModifyTable.scss';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import { dataSource } from '../../../../dataSources';

const ColumnEditorList = ({
  tableSchema,
  currentSchema,
  columnEdit,
  dispatch,
  readOnlyMode,
  validTypeCasts,
  dataTypeIndexMap,
  columnDefaultFunctions,
  customColumnNames,
}) => {
  const tableName = tableSchema.table_name;

  let pkLength = 0;
  const columnPKConstraints = {};
  if (tableSchema.primary_key) {
    pkLength = tableSchema.primary_key.columns.length;

    tableSchema.primary_key.columns.forEach(col => {
      columnPKConstraints[col] = tableSchema.primary_key.constraint_name;
    });
  }

  const columnUniqueConstraints = {};
  tableSchema.unique_constraints.forEach(uConst => {
    if (uConst.columns.length === 1) {
      columnUniqueConstraints[uConst.columns[0]] = uConst.constraint_name;
    }
  });

  const columns = tableSchema.columns.sort(ordinalColSort);

  /*
   * col.data_type_name contains internal representation of the data type
   * */
  return columns.map((col, i) => {
    const colName = col.column_name;
    const isArrayDataType = col.data_type === dataSource.columnDataTypes.ARRAY;
    // todo -- create getColumnProperties utility
    const getDisplayName = () => {
      if (isArrayDataType) {
        return col.data_type_name.replace('_', '') + '[]';
      }
      return dataSource.getColumnType(col);
    };
    const getType = () =>
      isArrayDataType
        ? col.data_type_name.replace('_', '') + '[]'
        : col.data_type_name;

    const columnProperties = {
      name: colName,
      tableName: col.table_name,
      schemaName: col.table_schema,
      display_type_name: getDisplayName(),
      type: getType(),
      isArrayDataType,
      isNullable: col.is_nullable === 'YES',
      isIdentity: col.is_identity,
      pkConstraint: columnPKConstraints[colName],
      isUnique: isColumnUnique(tableSchema, colName),
      default: col.column_default || '',
      comment: col.comment || '',
      customFieldName: customColumnNames[colName] || '',
      isOnlyPrimaryKey: columnPKConstraints[colName] && pkLength === 1,
    };

    const onSubmit = toggleEditor => {
      dispatch(saveColumnChangesSql(colName, col, toggleEditor));
    };

    const onDelete = () => {
      let confirmMessage = `This will permanently delete the column "${colName}" from this table`;
      if (columnProperties.pkConstraint) {
        confirmMessage = DELETE_PK_WARNING;
      }

      const isOk = getConfirmation(confirmMessage, true, colName);
      if (isOk) {
        dispatch(deleteColumnSql(col, tableSchema));
      }
    };
    const gqlCompatibilityWarning = () => {
      return (
        <GqlCompatibilityWarning
          identifier={colName}
          className={styles.add_mar_left_small}
          ifWarningCanBeFixed
        />
      );
    };

    const keyProperties = () => {
      const propertiesDisplay = [];

      const propertiesList = [];
      propertiesList.push(columnProperties.display_type_name);

      if (columnProperties.pkConstraint) {
        propertiesList.push('primary key');
      }

      if (columnProperties.isUnique || columnProperties.isOnlyPrimaryKey) {
        propertiesList.push('unique');
      }

      if (columnProperties.isIdentity) {
        propertiesList.push('identity');
      }

      if (columnProperties.isNullable) {
        propertiesList.push('nullable');
      }

      if (columnProperties.default) {
        propertiesList.push(`default: ${columnProperties.default}`);
      }

      const keyPropertiesString = propertiesList.join(', ');

      propertiesDisplay.push(<i key={'props'}>{keyPropertiesString}</i>);

      propertiesDisplay.push(<br key={'br1'} />);

      propertiesDisplay.push(
        <span key={'comment'} className={styles.text_gray}>
          {columnProperties.comment && `${columnProperties.comment}`}
        </span>
      );

      return propertiesDisplay;
    };

    const collapsedLabel = () => {
      return (
        <div key={colName}>
          <b>
            {colName}
            <i>
              {columnProperties.customFieldName &&
                ` → ${columnProperties.customFieldName}`}
            </i>
          </b>{' '}
          {gqlCompatibilityWarning()} - {keyProperties()}
        </div>
      );
    };

    const expandedLabel = () => {
      return (
        <div key={colName}>
          <b>{colName}</b>
        </div>
      );
    };

    /* If the dataTypeIndexMap is not loaded, then just load the current type information
     * */

    const getValidTypeCasts = udtName => {
      if (isArrayDataType) {
        udtName = udtName.replace('_', '');
      }
      const lowerUdtName = udtName.toLowerCase();
      if (lowerUdtName in validTypeCasts) {
        return validTypeCasts[lowerUdtName];
      }
      if (dataTypeIndexMap && dataTypeIndexMap[lowerUdtName]) {
        return [
          ...dataTypeIndexMap[lowerUdtName],
          ...dataTypeIndexMap[defaultDataTypeToCast],
        ];
      }
      return [lowerUdtName, lowerUdtName, ''];
    };

    const getValidDefaultTypes = udtName => {
      const lowerUdtName = udtName.toLowerCase();
      let defaultOptions = [];
      if (lowerUdtName in columnDefaultFunctions) {
        defaultOptions = columnDefaultFunctions[lowerUdtName];
      } else {
        defaultOptions = inferDefaultValues(
          columnDefaultFunctions,
          validTypeCasts
        )(lowerUdtName);
      }

      return getDefaultFunctionsOptions(defaultOptions);
    };

    /*
     * Alter type options contains a list of items and its valid castable types
     * [
     *  "Data type",
     *  "User friendly name of the data type",
     *  "Description of the data type",
     *  "Comma separated castable data types",
     *  "Comma separated user friendly names of the castable data types",
     *  "Colon separated user friendly description of the castable data types"
     *  ]
     * */

    const colEditorExpanded = () => {
      return (
        <ColumnEditor
          alterTypeOptions={getValidTypeCasts(
            col.data_type_name,
            isArrayDataType
          )}
          defaultOptions={getValidDefaultTypes(col.data_type_name)}
          column={col}
          onSubmit={onSubmit}
          tableName={tableName}
          dispatch={dispatch}
          currentSchema={currentSchema}
          columnProperties={columnProperties}
          selectedProperties={columnEdit}
          editColumn={editColumn}
        />
      );
    };

    const editorExpandCallback = () => {
      dispatch(setColumnEdit(columnProperties));
    };

    const editorCollapseCallback = () => {
      dispatch(resetColumnEdit(colName));
    };

    return (
      <div key={colName}>
        <ExpandableEditor
          editorExpanded={colEditorExpanded}
          property={`column-${i}`}
          service="modify-table"
          saveFunc={onSubmit}
          readOnlyMode={readOnlyMode}
          removeFunc={columnProperties.pkConstraint ? null : onDelete}
          collapsedClass={styles.display_flex}
          expandedLabel={expandedLabel}
          collapsedLabel={collapsedLabel}
          expandCallback={editorExpandCallback}
          collapseCallback={editorCollapseCallback}
        />
      </div>
    );
  });
};

export default ColumnEditorList;
