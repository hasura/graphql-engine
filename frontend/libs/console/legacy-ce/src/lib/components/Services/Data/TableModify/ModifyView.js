import PropTypes from 'prop-types';
import React from 'react';
import { Analytics, REDACT_EVERYTHING } from '../../../../features/Analytics';
import { Button } from '../../../../new-components/Button';
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
import { NotFoundError } from '../../../Error/PageNotFound';

import { getConfirmation } from '../../../Common/utils/jsUtils';
import Tooltip from '../../../Common/Tooltip/Tooltip';
import {
  getTableCustomColumnNames,
  findTable,
  generateTableDef,
  isFeatureSupported,
} from '../../../../dataSources';
import ViewDefinitions from './ViewDefinitions';
import { RightContainer } from '../../../Common/Layout/RightContainer';
import ComputedFields from './ComputedFields';
import RootFields from './RootFields';
import FeatureDisabled from '../FeatureDisabled';
import { inputStyles } from '../constants';

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
    currentSource,
  } = props;

  React.useEffect(() => {
    dispatch({ type: RESET });
    dispatch(setTable(tableName));
    dispatch(fetchViewDefinition(tableName, false));
  }, [dispatch, tableName]);

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
    setCustomColumnNames(getTableCustomColumnNames(tableSchema));
  }, [tableSchema.configuration]);

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
      const columnName = c.column_name;

      const setCustomColumnName = e => {
        const value = e.target.value;
        setCustomColumnNames({
          ...customColumnNames,
          [columnName]: value,
        });
      };

      const columnExpanded = () => {
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
                type="text"
                className={inputStyles}
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
        <h4 className="flex items-center text-gray-600 font-semibold mb-formlabel">
          Columns
        </h4>
        <div className="w-full sm:w-6/12 mb-md">{columnList}</div>
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
      className="mr-sm"
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
      mode="destructive"
      size="sm"
      onClick={deleteOnClick}
      data-test="delete-view"
    >
      Delete view
    </Button>
  );

  if (
    !isFeatureSupported('tables.modify.enabled') ||
    isFeatureSupported('tables.modify.readOnly')
  ) {
    return (
      <FeatureDisabled
        tab="modify"
        tableName={tableName}
        schemaName={currentSchema}
        tableType={tableType}
      />
    );
  }

  return (
    <RightContainer>
      <Analytics name="ModifyTableView" {...REDACT_EVERYTHING}>
        <div>
          <TableHeader
            dispatch={dispatch}
            table={tableSchema}
            tabName="modify"
            migrationMode={migrationMode}
            readOnlyMode={readOnlyMode}
            source={currentSource}
          />
          <br />
          <div className="w-full sm:w-6/12 mb-lg">
            <h4 className="flex items-center text-gray-600 font-semibold mb-formlabel">
              View Comments
            </h4>
            <TableCommentEditor
              tableComment={tableComment}
              tableCommentEdit={tableCommentEdit}
              tableType={tableType}
              dispatch={dispatch}
            />
          </div>

          <h3 className="text-sm tracking-widest text-gray-400 uppercase font-semibold mb-sm">
            View Properties
          </h3>

          <ViewDefinitions
            dispatch={dispatch}
            sql={viewDefSql}
            source={currentSource}
          />

          {getViewColumnsSection()}

          {isFeatureSupported('tables.modify.computedFields') && (
            <div className="w-full sm:w-6/12 mb-md">
              <ComputedFields tableSchema={tableSchema} />
            </div>
          )}

          {isFeatureSupported('tables.modify.customGqlRoot') && (
            <div className="w-full sm:w-6/12 mb-md">
              <RootFields tableSchema={tableSchema} />
            </div>
          )}

          {untrackBtn}
          {deleteBtn}
          <br />
          <br />
          <div className="top-150 r-50 bottom-0 min-w-13 grid-cols-3">
            {alert}
          </div>
        </div>
      </Analytics>
    </RightContainer>
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
    currentSource: state.tables.currentDataSource,
    allSchemas: state.tables.allSchemas,
    migrationMode: state.main.migrationMode,
    readOnlyMode: state.main.readOnlyMode,
    serverVersion: state.main.serverVersion,
    ...state.tables.modify,
  };
};

const modifyViewConnector = connect => connect(mapStateToProps)(ModifyView);

export default modifyViewConnector;
