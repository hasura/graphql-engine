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
} from '../TableModify/ModifyActions';
import { fetchColumnComment } from '../DataActions';
import { ordinalColSort } from '../utils';

import styles from './ModifyTable.scss';

const ColumnEditorList = ({
  tableSchema,
  currentSchema,
  allowRename,
  columnEdit,
  dispatch,
  columnComments,
}) => {
  const tablePrimaryKeyColumns = tableSchema.primary_key
    ? tableSchema.primary_key.columns
    : [];

  const tableName = tableSchema.table_name;
  const columns = tableSchema.columns.sort(ordinalColSort);

  return columns.map((c, i) => {
    const colName = c.column_name;
    const columnProperties = {
      name: c.column_name,
      tableName: c.table_name,
      schemaName: c.table_schema,
      type: c.data_type,
      isNullable: c.is_nullable === 'YES' ? true : false,
      isPrimaryKey: tablePrimaryKeyColumns.includes(c.column_name),
      isUnique: false,
      default: c.column_default || '',
    };

    for (let uci = tableSchema.unique_constraints.length - 1; uci >= 0; uci--) {
      const constraint = tableSchema.unique_constraints[uci];
      if (
        constraint.columns.length === 1 &&
        constraint.columns[0] === c.column_name
      ) {
        columnProperties.isUnique = true;
      }
    }

    const onSubmit = () => {
      dispatch(saveColumnChangesSql(colName, c, allowRename));
    };

    const onDelete = () => {
      const isOk = confirm('Are you sure you want to delete?');
      if (isOk) {
        dispatch(deleteColumnSql(tableName, colName, c));
      }
    };

    const safeOnDelete = () => {
      let confirmMessage = 'Are you sure you want to delete?';
      if (columnProperties.isPrimaryKey) {
        confirmMessage = DELETE_PK_WARNING;
      }
      const isOk = window.confirm(confirmMessage);
      if (isOk) {
        dispatch(deleteColumnSql(tableName, colName, c));
      }
    };

    const keyProperties = () => {
      const propertiesList = [];
      if (columnProperties.isPrimaryKey) propertiesList.push('primary key');
      if (columnProperties.isNullable) propertiesList.push('nullable');
      if (columnProperties.isUnique) propertiesList.push('unique');
      const keyPropertiesString = propertiesList.join(', ');
      return <i>{keyPropertiesString && `- ${keyPropertiesString}`}</i>;
    };

    const collapsedLabel = () => {
      return (
        <div key={colName}>
          <div className="container-fluid">
            <div className="row">
              <h5 className={styles.padd_bottom}>
                <b>{colName}</b> {keyProperties()}
                &nbsp;
              </h5>
            </div>
          </div>
        </div>
      );
    };

    const expandedLabel = () => {
      return (
        <div key={colName}>
          <div className="container-fluid">
            <div className="row">
              <h5 className={styles.padd_bottom}>
                <b>{colName}</b>
                &nbsp;
              </h5>
            </div>
          </div>
        </div>
      );
    };

    const colEditorExpanded = () => {
      return (
        <ColumnEditor
          column={c}
          onSubmit={onSubmit}
          onDelete={safeOnDelete}
          tableName={tableName}
          dispatch={dispatch}
          currentSchema={currentSchema}
          columnComment={columnComments[c.column_name]}
          allowRename={allowRename}
          columnProperties={columnProperties}
          selectedProperties={columnEdit}
          editColumn={editColumn}
        />
      );
    };

    const editorExpandCallback = () => {
      dispatch(setColumnEdit(columnProperties));
      dispatch(fetchColumnComment(tableName, colName));
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
          removeFunc={columnProperties.isPrimaryKey ? null : onDelete}
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
