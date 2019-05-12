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
  const tableName = tableSchema.table_name;

  const columnPKConstraints = {};
  if (tableSchema.primary_key) {
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

  return columns.map((col, i) => {
    const colName = col.column_name;

    const columnProperties = {
      name: colName,
      tableName: col.table_name,
      schemaName: col.table_schema,
      type: col.data_type !== 'USER-DEFINED' ? col.data_type : col.udt_name,
      isNullable: col.is_nullable === 'YES',
      pkConstraint: columnPKConstraints[colName],
      isUnique: columnUniqueConstraints[colName] ? true : false,
      // uniqueConstraint: columnUniqueConstraints[colName],
      default: col.column_default || '',
    };

    const onSubmit = () => {
      dispatch(saveColumnChangesSql(colName, col, allowRename));
    };

    const onDelete = () => {
      const isOk = confirm('Are you sure you want to delete?');
      if (isOk) {
        dispatch(deleteColumnSql(tableName, colName, col));
      }
    };

    const safeOnDelete = () => {
      let confirmMessage = 'Are you sure you want to delete?';
      if (columnProperties.pkConstraint) {
        confirmMessage = DELETE_PK_WARNING;
      }
      const isOk = window.confirm(confirmMessage);
      if (isOk) {
        dispatch(deleteColumnSql(tableName, colName, col));
      }
    };

    const keyProperties = () => {
      const propertiesList = [];

      propertiesList.push(columnProperties.type);

      if (columnProperties.pkConstraint) {
        propertiesList.push('primary key');
      }

      if (columnProperties.isUnique) {
        propertiesList.push('unique');
      }

      if (columnProperties.isNullable) {
        propertiesList.push('nullable');
      }

      if (columnProperties.default) {
        propertiesList.push(`default: ${columnProperties.default}`);
      }

      const keyPropertiesString = propertiesList.join(', ');

      return <i>{keyPropertiesString && `- ${keyPropertiesString}`}</i>;
    };

    const collapsedLabel = () => {
      return (
        <div key={colName}>
          <b>{colName}</b> {keyProperties()}
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

    const colEditorExpanded = () => {
      return (
        <ColumnEditor
          column={col}
          onSubmit={onSubmit}
          onDelete={safeOnDelete}
          tableName={tableName}
          dispatch={dispatch}
          currentSchema={currentSchema}
          columnComment={columnComments[col.column_name]}
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
