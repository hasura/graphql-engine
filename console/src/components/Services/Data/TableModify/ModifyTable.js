import PropTypes from 'prop-types';
import React from 'react';

import TableHeader from '../TableCommon/TableHeader';
import { getAllDataTypeMap } from '../Common/utils';
import {
  deleteTableSql,
  untrackTableSql,
  RESET,
  setUniqueKeys,
  toggleTableAsEnum,
} from '../TableModify/ModifyActions';
import {
  setTable,
  fetchColumnTypeInfo,
  RESET_COLUMN_TYPE_INFO,
  fetchFunctionInit,
} from '../DataActions';
import Button from '../../../Common/Button/Button';
import ColumnEditorList from './ColumnEditorList';
import ColumnCreator from './ColumnCreator';
import PrimaryKeyEditor from './PrimaryKeyEditor';
import TableCommentEditor from './TableCommentEditor';
import EnumsSection, {
  EnumTableModifyWarning,
} from '../Common/Components/EnumsSection';
import ForeignKeyEditor from './ForeignKeyEditor';
import UniqueKeyEditor from './UniqueKeyEditor';
import TriggerEditorList from './TriggerEditorList';
import CheckConstraints from './CheckConstraints';
import RootFields from './RootFields';
import styles from './ModifyTable.scss';
import { NotFoundError } from '../../../Error/PageNotFound';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import {
  currentDriver,
  driverToLabel,
  findTable,
  generateTableDef,
  getTableCustomColumnNames,
  isFeatureSupported,
} from '../../../../dataSources';
import Tooltip from '../../../Common/Tooltip/Tooltip';
import {
  foreignKeyDescription,
  primaryKeyDescription,
  uniqueKeyDescription,
  checkConstraintsDescription,
} from '../Common/TooltipMessages';
import { RightContainer } from '../../../Common/Layout/RightContainer';
import { NotSupportedNote } from '../../../Common/NotSupportedNote';
import ConnectedComputedFields from './ComputedFields';
import FeatureDisabled from '../FeatureDisabled';
import IndexFields from './IndexFields';
import PartitionInfo from './PartitionInfo';

class ModifyTable extends React.Component {
  componentDidMount() {
    if (!isFeatureSupported('tables.modify.enabled')) return;
    const { dispatch } = this.props;
    dispatch({ type: RESET });
    dispatch(setTable(this.props.tableName));
    if (!isFeatureSupported('tables.modify.readOnly'))
      dispatch(fetchColumnTypeInfo());
    dispatch(fetchFunctionInit());
  }

  componentWillUnmount() {
    this.props.dispatch({
      type: RESET_COLUMN_TYPE_INFO,
    });
  }

  render() {
    const {
      tableName,
      allTables,
      dispatch,
      migrationMode,
      readOnlyMode,
      currentSchema,
      tableCommentEdit,
      columnEdit,
      pkModify,
      fkModify,
      checkConstraintsModify,
      dataTypes,
      validTypeCasts,
      uniqueKeyModify,
      columnDefaultFunctions,
      schemaList,
      tableEnum,
      postgresVersion,
      currentSource,
    } = this.props;

    if (!isFeatureSupported('tables.modify.enabled')) {
      return (
        <FeatureDisabled
          tab="modify"
          tableName={tableName}
          schemaName={currentSchema}
        />
      );
    }

    const dataTypeIndexMap = getAllDataTypeMap(dataTypes);

    const table = findTable(
      allTables,
      generateTableDef(tableName, currentSchema)
    );

    if (!table && isFeatureSupported('tables.modify.enabled')) {
      throw new NotFoundError();
    }

    const tableComment = table.comment;

    const untrackBtn = (
      <Button
        type="submit"
        className={styles.add_mar_right}
        color="white"
        size="sm"
        onClick={() => {
          const confirmMessage = `This will remove the table "${tableName}" from the GraphQL schema`;
          const isOk = getConfirmation(confirmMessage);
          if (isOk) {
            dispatch(untrackTableSql(tableName));
          }
        }}
        data-test="untrack-table"
      >
        Untrack Table
      </Button>
    );

    const deleteBtn = (
      <Button
        type="submit"
        color="red"
        size="sm"
        onClick={() => {
          const confirmMessage = `This will permanently delete the table "${tableName}" from the database`;
          const isOk = getConfirmation(confirmMessage, true, tableName);
          if (isOk) {
            dispatch(deleteTableSql(tableName, table));
          }
        }}
        data-test="delete-table"
      >
        Delete table
      </Button>
    );

    const getEnumsSection = () => {
      const toggleEnum = () => dispatch(toggleTableAsEnum(table.is_enum));

      return (
        <React.Fragment>
          <EnumsSection
            isEnum={table.is_enum}
            toggleEnum={toggleEnum}
            loading={tableEnum.loading}
          />
          <hr className="my-md" />
        </React.Fragment>
      );
    };

    return (
      <RightContainer>
        <div className={`${styles.container} container-fluid`}>
          <TableHeader
            dispatch={dispatch}
            table={table}
            source={currentSource}
            tabName="modify"
            migrationMode={migrationMode}
            readOnlyMode={readOnlyMode}
          />
          <br />
          <div className={`container-fluid ${styles.padd_left_remove}`}>
            <div
              className={`col-xs-10 ${styles.padd_left_remove} ${styles.modifyMinWidth}`}
            >
              {isFeatureSupported('tables.modify.readOnly') && (
                <div className={styles.readOnly}>
                  <p className={styles.readOnlyText}>
                    <i className="fa fa-flask" aria-hidden="true" /> Coming soon
                    for {driverToLabel[currentDriver]}
                  </p>
                  <p className={styles.noMargin}>
                    This page is currently read-only, but we're actively working
                    on making it available for the Console.
                  </p>
                </div>
              )}

              {isFeatureSupported('tables.modify.comments.view') && (
                <>
                  <TableCommentEditor
                    tableComment={tableComment}
                    tableCommentEdit={tableCommentEdit}
                    tableType="TABLE"
                    dispatch={dispatch}
                    readOnly={
                      !isFeatureSupported('tables.modify.comments.edit')
                    }
                  />
                  <EnumTableModifyWarning isEnum={table.is_enum} />
                </>
              )}

              {isFeatureSupported('tables.modify.columns.view') && (
                <>
                  <h4 className={styles.subheading_text}>Columns</h4>
                  <ColumnEditorList
                    validTypeCasts={validTypeCasts}
                    dataTypeIndexMap={dataTypeIndexMap}
                    tableSchema={table}
                    columnEdit={columnEdit}
                    dispatch={dispatch}
                    readOnlyMode={
                      !isFeatureSupported('tables.modify.columns.edit')
                    }
                    currentSchema={currentSchema}
                    columnDefaultFunctions={columnDefaultFunctions}
                    customColumnNames={getTableCustomColumnNames(table)}
                  />
                </>
              )}
              {isFeatureSupported('tables.modify.columns.edit') && (
                <>
                  <ColumnCreator
                    dispatch={dispatch}
                    tableName={tableName}
                    dataTypes={dataTypes}
                    validTypeCasts={validTypeCasts}
                    columnDefaultFunctions={columnDefaultFunctions}
                    postgresVersion={postgresVersion}
                  />
                  <hr className="my-md" />
                </>
              )}

              {isFeatureSupported('tables.modify.computedFields') && (
                <>
                  <ConnectedComputedFields tableSchema={table} />
                  <hr className="my-md" />
                </>
              )}

              {isFeatureSupported('tables.modify.primaryKeys.view') && (
                <>
                  <h4 className={styles.subheading_text}>
                    Primary Key &nbsp; &nbsp;
                    <Tooltip message={primaryKeyDescription} />
                  </h4>
                  <PrimaryKeyEditor
                    tableSchema={table}
                    readOnlyMode={
                      !isFeatureSupported('tables.modify.primaryKeys.edit')
                    }
                    pkModify={pkModify}
                    dispatch={dispatch}
                    currentSchema={currentSchema}
                  />
                  <hr className="my-md" />
                </>
              )}

              {isFeatureSupported('tables.modify.foreignKeys.view') && (
                <>
                  <h4 className={styles.subheading_text}>
                    Foreign Keys &nbsp; &nbsp;
                    <Tooltip message={foreignKeyDescription} />
                  </h4>
                  <ForeignKeyEditor
                    tableSchema={table}
                    currentSchema={currentSchema}
                    allSchemas={allTables}
                    schemaList={schemaList}
                    dispatch={dispatch}
                    fkModify={fkModify}
                    readOnlyMode={
                      !isFeatureSupported('tables.modify.foreignKeys.edit')
                    }
                  />
                  <hr className="my-md" />
                </>
              )}

              {isFeatureSupported('tables.modify.uniqueKeys.view') && (
                <>
                  <h4 className={styles.subheading_text}>
                    Unique Keys &nbsp; &nbsp;
                    <Tooltip message={uniqueKeyDescription} />
                  </h4>
                  <UniqueKeyEditor
                    tableSchema={table}
                    currentSchema={currentSchema}
                    allSchemas={allTables}
                    dispatch={dispatch}
                    uniqueKeys={uniqueKeyModify}
                    setUniqueKeys={setUniqueKeys}
                    readOnlyMode={
                      !isFeatureSupported('tables.modify.uniqueKeys.edit')
                    }
                  />
                  <hr className="my-md" />
                </>
              )}
              {isFeatureSupported('tables.modify.indexes.view') ? (
                <>
                  <IndexFields tableSchema={table} />
                  <hr />
                </>
              ) : null}
              {isFeatureSupported('tables.modify.triggers') && (
                <>
                  <div className={styles.add_mar_bottom}>
                    <h4 className={styles.subheading_text_no_padd}>Triggers</h4>
                    <NotSupportedNote unsupported={['mysql']} />
                  </div>
                  <TriggerEditorList tableSchema={table} dispatch={dispatch} />
                  <hr className="my-md" />
                </>
              )}
              {isFeatureSupported('tables.modify.checkConstraints.view') && (
                <>
                  <div className={styles.add_mar_bottom}>
                    <h4 className={styles.subheading_text_no_padd}>
                      Check Constraints &nbsp; &nbsp;
                      <Tooltip message={checkConstraintsDescription} />
                    </h4>
                    <NotSupportedNote unsupported={['mysql']} />
                  </div>
                  <CheckConstraints
                    constraints={table.check_constraints}
                    checkConstraintsModify={checkConstraintsModify}
                    dispatch={dispatch}
                    readOnlyMode={
                      !isFeatureSupported('tables.modify.checkConstraints.edit')
                    }
                  />
                  <hr className="my-md" />
                </>
              )}
              {table.table_type === 'PARTITIONED TABLE' && (
                <PartitionInfo table={table} dispatch={dispatch} />
              )}
              {isFeatureSupported('tables.modify.customGqlRoot') && (
                <>
                  <RootFields tableSchema={table} />
                  <hr className="my-md" />
                </>
              )}
              {isFeatureSupported('tables.modify.setAsEnum') &&
                getEnumsSection()}
              {isFeatureSupported('tables.modify.untrack') && untrackBtn}
              {isFeatureSupported('tables.modify.delete') && deleteBtn}
              <br />
              <br />
            </div>
          </div>
        </div>
      </RightContainer>
    );
  }
}

ModifyTable.propTypes = {
  tableName: PropTypes.string.isRequired,
  currentSchema: PropTypes.string.isRequired,
  allTables: PropTypes.array.isRequired,
  migrationMode: PropTypes.bool.isRequired,
  readOnlyMode: PropTypes.bool.isRequired,
  activeEdit: PropTypes.object.isRequired,
  fkAdd: PropTypes.object.isRequired,
  relAdd: PropTypes.object.isRequired,
  ongoingRequest: PropTypes.bool.isRequired,
  lastError: PropTypes.object,
  lastFormError: PropTypes.object,
  columnEdit: PropTypes.object.isRequired,
  lastSuccess: PropTypes.bool,
  dispatch: PropTypes.func.isRequired,
  pkModify: PropTypes.array.isRequired,
  fkModify: PropTypes.array.isRequired,
  serverVersion: PropTypes.string,
};

const mapStateToProps = (state, ownProps) => ({
  tableName: ownProps.params.table,
  allTables: state.tables.allSchemas,
  migrationMode: state.main.migrationMode,
  readOnlyMode: state.main.readOnlyMode,
  serverVersion: state.main.serverVersion,
  currentSchema: state.tables.currentSchema,
  columnEdit: state.tables.modify.columnEdit,
  pkModify: state.tables.modify.pkModify,
  fkModify: state.tables.modify.fkModify,
  dataTypes: state.tables.columnDataTypes,
  columnDefaultFunctions: state.tables.columnDefaultFunctions,
  validTypeCasts: state.tables.columnTypeCasts,
  columnDataTypeFetchErr: state.tables.columnDataTypeFetchErr,
  schemaList: state.tables.schemaList,
  postgresVersion: state.main.postgresVersion,
  currentSource: state.tables.currentDataSource,
  ...state.tables.modify,
});

const modifyTableConnector = connect => connect(mapStateToProps)(ModifyTable);

export default modifyTableConnector;
