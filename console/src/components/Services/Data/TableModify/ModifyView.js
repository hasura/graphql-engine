import PropTypes from 'prop-types';
import React from 'react';
import TableHeader from '../TableCommon/TableHeader';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import {
  fetchViewDefinition,
  deleteViewSql,
  untrackTableSql,
  RESET,
  setViewCustomColumnNames,
} from './ModifyActions';
import TableCommentEditor from './TableCommentEditor';
import { ordinalColSort } from '../utils';
import { setTable } from '../DataActions';
import Button from '../../../Common/Button/Button';
import { NotFoundError } from '../../../Error/PageNotFound';

import { getConfirmation } from '../../../Common/utils/jsUtils';
import {
  findTable,
  generateTableDef,
  getColumnName,
  getTableCustomColumnNames,
} from '../../../Common/utils/pgUtils';
import Tooltip from '../../../Common/Tooltip/Tooltip';
import styles from './ModifyTable.scss';
import ViewDefinitions from './ViewDefinitions';
import ComputedFields from './ComputedFields';
import RootFields from './RootFields';

const ModifyView = props => {
  const {
    viewDefSql,
    tableName,
    tableType,
    allSchemas,
    ongoingRequest,
    lastError,
    lastSuccess,
    dispatch,
    currentSchema,
    tableCommentEdit,
    migrationMode,
    readOnlyMode,
  } = props;

  React.useEffect(() => {
    dispatch({ type: RESET });
    dispatch(setTable(tableName));
    dispatch(fetchViewDefinition(tableName, false));
  }, []);

  const tableSchema = findTable(
    allSchemas,
    generateTableDef(tableName, currentSchema)
  );

  const [customColumnNames, setCustomColumnNames] = React.useState({});
  const existingCustomColumnNames = getTableCustomColumnNames(tableSchema);
  const initCustomColumnNames = () => {
    setCustomColumnNames(existingCustomColumnNames);
  };

  React.useEffect(() => {
    initCustomColumnNames();
  }, [existingCustomColumnNames]);

  if (!tableSchema) {
    // throw a 404 exception
    throw new NotFoundError();
  }

  const tableComment = tableSchema.comment;

  let alert = null;
  if (ongoingRequest) {
    alert = (
      <div
        className="hidden alert alert-warning alert-dismissable"
        role="alert"
      >
        Saving...
      </div>
    );
  } else if (lastError) {
    alert = (
      <div className="hidden alert alert-danger" role="alert">
        Error: {JSON.stringify(lastError)}
      </div>
    );
  } else if (lastSuccess) {
    alert = (
      <div className="hidden alert alert-success" role="alert">
        Saved!
      </div>
    );
  }

  const getViewColumnsSection = () => {
    const columns = tableSchema.columns.sort(ordinalColSort);

    const columnList = columns.map((c, i) => {
      const columnName = getColumnName(c);

      const setCustomColumnName = e => {
        const value = e.target.value;
        setCustomColumnNames({
          ...customColumnNames,
          [columnName]: value,
        });
      };

      const columnExpanded = () => {
        return (
          <div className={`${styles.display_flex}`}>
            <label className={'col-xs-4'}>
              GraphQL field name
              <Tooltip
                message={
                  'Expose the column with a different name in the GraphQL API'
                }
              />
            </label>
            <div className={'col-xs-6'}>
              <input
                type="text"
                className={'form-control'}
                value={customColumnNames[columnName] || ''}
                placeholder={`${columnName} (default)`}
                onChange={setCustomColumnName}
              />
            </div>
          </div>
        );
      };

      const label = () => {
        return (
          <b>
            {columnName}
            {existingCustomColumnNames[columnName]
              ? ` → ${existingCustomColumnNames[columnName]}`
              : ''}
          </b>
        );
      };

      const saveFunc = toggle => {
        dispatch(
          setViewCustomColumnNames(
            customColumnNames,
            tableName,
            currentSchema,
            toggle
          )
        );
      };

      return (
        <div key={i}>
          <ExpandableEditor
            editorExpanded={columnExpanded}
            property={`view-column-${i}`}
            service="modify-view"
            expandedLabel={label}
            saveFunc={saveFunc}
            collapsedLabel={label}
            expandCallback={initCustomColumnNames}
            collapseCallback={initCustomColumnNames}
          />
        </div>
      );
    });

    return (
      <>
        <h4 className={styles.subheading_text}>Columns</h4>
        {columnList}
      </>
    );
  };

  const untrackOnclick = () => {
    const confirmMessage = `This will remove the view "${tableName}" from the GraphQL schema`;
    const isOk = getConfirmation(confirmMessage);
    if (isOk) {
      dispatch(untrackTableSql(tableName));
    }
  };

  const untrackBtn = (
    <Button
      type="submit"
      className={styles.add_mar_right}
      color="white"
      size="sm"
      onClick={untrackOnclick}
      data-test="untrack-view"
    >
      Untrack View
    </Button>
  );

  const deleteOnClick = () => {
    const confirmMessage = `This will permanently delete the view "${tableName}" from the database`;
    const isOk = getConfirmation(confirmMessage, true, tableName);
    if (isOk) {
      dispatch(deleteViewSql(tableName, tableType));
    }
  };
  const deleteBtn = (
    <Button
      type="submit"
      color="red"
      size="sm"
      onClick={deleteOnClick}
      data-test="delete-view"
    >
      Delete view
    </Button>
  );

  return (
    <div className={styles.container + ' container-fluid'}>
      <TableHeader
        dispatch={dispatch}
        table={tableSchema}
        tabName="modify"
        migrationMode={migrationMode}
        readOnlyMode={readOnlyMode}
      />
      <br />
      <div className={'container-fluid ' + styles.padd_left_remove}>
        <div className={'col-xs-8 ' + styles.padd_left_remove}>
          <TableCommentEditor
            tableComment={tableComment}
            tableCommentEdit={tableCommentEdit}
            tableType={tableType}
            dispatch={dispatch}
          />
          <ViewDefinitions dispatch={dispatch} sql={viewDefSql} />
          <hr />
          {getViewColumnsSection()}
          <hr />
          <ComputedFields tableSchema={tableSchema} />
          <hr />
          <RootFields tableSchema={tableSchema} />
          <hr />
          {untrackBtn}
          {deleteBtn}
          <br />
          <br />
        </div>
        <div className={styles.fixed + ' col-xs-3 hidden'}>{alert}</div>
      </div>
    </div>
  );
};

ModifyView.propTypes = {
  sql: PropTypes.string.isRequired,
  tableName: PropTypes.string.isRequired,
  tableType: PropTypes.string.isRequired,
  allSchemas: PropTypes.array.isRequired,
  currentSchema: PropTypes.string.isRequired,
  activeEdit: PropTypes.object.isRequired,
  ongoingRequest: PropTypes.bool.isRequired,
  migrationMode: PropTypes.bool.isRequired,
  readOnlyMode: PropTypes.bool.isRequired,
  lastError: PropTypes.object,
  lastSuccess: PropTypes.bool,
  dispatch: PropTypes.func.isRequired,
  serverVersion: PropTypes.string,
};

const findViewType = (currentSchema, viewName, allItems) => {
  for (const item of allItems) {
    if (item.table_schema === currentSchema && item.table_name === viewName) {
      return item.table_type;
    }
  }
  return 'VIEW';
};

const mapStateToProps = (state, ownProps) => {
  const tableName = ownProps.params.table;
  const schemaName = state.tables.currentSchema;
  const tableType = findViewType(
    schemaName,
    tableName,
    state.tables.allSchemas
  );
  return {
    tableName,
    tableType,
    currentSchema: schemaName,
    allSchemas: state.tables.allSchemas,
    migrationMode: state.main.migrationMode,
    readOnlyMode: state.main.readOnlyMode,
    serverVersion: state.main.serverVersion,
    ...state.tables.modify,
  };
};

const modifyViewConnector = connect => connect(mapStateToProps)(ModifyView);

export default modifyViewConnector;
