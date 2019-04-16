import PropTypes from 'prop-types';
import React from 'react';
import TableHeader from '../TableCommon/TableHeader';
import {
  deleteTableSql,
  untrackTableSql,
  RESET,
} from '../TableModify/ModifyActions';
import { setTable, fetchTableComment } from '../DataActions';
import Button from '../../../Common/Button/Button';
import ColumnEditorList from './ColumnEditorList';
import ColumnCreator from './ColumnCreator';
import PrimaryKeyEditor from './PrimaryKeyEditor';
import TableCommentEditor from './TableCommentEditor';
import ForeignKeyEditor from './ForeignKeyEditor';
import semverCheck from '../../../../helpers/semver';
import styles from './ModifyTable.scss';

class ModifyTable extends React.Component {
  state = {
    supportTableColumnRename: false,
  };

  componentDidMount() {
    const { dispatch, serverVersion } = this.props;
    dispatch({ type: RESET });
    dispatch(setTable(this.props.tableName));
    dispatch(fetchTableComment(this.props.tableName));
    if (serverVersion) {
      this.checkTableColumnRenameSupport(serverVersion);
    }
  }

  componentWillReceiveProps(nextProps) {
    if (
      nextProps.serverVersion &&
      nextProps.serverVersion !== this.props.serverVersion
    ) {
      this.checkTableColumnRenameSupport(nextProps.serverVersion);
    }
  }

  checkTableColumnRenameSupport = serverVersion => {
    try {
      if (semverCheck('tableColumnRename', serverVersion)) {
        this.setState({
          supportTableColumnRename: true,
        });
      }
    } catch (e) {
      console.error(e);
    }
  };

  render() {
    const {
      tableName,
      allSchemas,
      dispatch,
      migrationMode,
      currentSchema,
      tableComment,
      columnComments,
      tableCommentEdit,
      columnEdit,
      pkModify,
      fkModify,
    } = this.props;
    const tableSchema = allSchemas.find(t => t.table_name === tableName);

    const untrackBtn = (
      <Button
        type="submit"
        className={styles.add_mar_right}
        color="white"
        size="sm"
        onClick={() => {
          const isOk = confirm('Are you sure to untrack?');
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
          const isOk = confirm('Are you sure?');
          if (isOk) {
            dispatch(deleteTableSql(tableName, tableSchema));
          }
        }}
        data-test="delete-table"
      >
        Delete table
      </Button>
    );

    return (
      <div className={`${styles.container} container-fluid`}>
        <TableHeader
          dispatch={dispatch}
          tableName={tableName}
          tabName="modify"
          migrationMode={migrationMode}
          currentSchema={currentSchema}
          allowRename={this.state.supportTableColumnRename}
        />
        <br />
        <div className={`container-fluid ${styles.padd_left_remove}`}>
          <div
            className={
              `col-xs-10 ${styles.padd_left_remove}` +
              ' ' +
              styles.modifyMinWidth
            }
          >
            <TableCommentEditor
              tableComment={tableComment}
              tableCommentEdit={tableCommentEdit}
              dispatch={dispatch}
            />
            <h4 className={styles.subheading_text}>Columns</h4>
            <ColumnEditorList
              tableSchema={tableSchema}
              columnEdit={columnEdit}
              allowRename={this.state.supportTableColumnRename}
              columnComments={columnComments}
              dispatch={dispatch}
              currentSchema={currentSchema}
            />
            <hr />
            <h4 className={styles.subheading_text}>Add a new column</h4>
            <ColumnCreator dispatch={dispatch} tableName={tableName} />
            <hr />
            <h4 className={styles.subheading_text}>Primary Key</h4>
            <PrimaryKeyEditor
              tableSchema={tableSchema}
              pkModify={pkModify}
              dispatch={dispatch}
              currentSchema={currentSchema}
            />
            <hr />
            <h4 className={styles.subheading_text}>Foreign Keys</h4>
            <ForeignKeyEditor
              tableSchema={tableSchema}
              currentSchema={currentSchema}
              allSchemas={allSchemas}
              dispatch={dispatch}
              fkModify={fkModify}
            />
            <hr />
            {untrackBtn}
            {deleteBtn}
            <br />
            <br />
          </div>
        </div>
      </div>
    );
  }
}

ModifyTable.propTypes = {
  tableName: PropTypes.string.isRequired,
  currentSchema: PropTypes.string.isRequired,
  allSchemas: PropTypes.array.isRequired,
  migrationMode: PropTypes.bool.isRequired,
  tableComment: PropTypes.string.isRequired,
  columnComments: PropTypes.string.isRequired,
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
  allSchemas: state.tables.allSchemas,
  migrationMode: state.main.migrationMode,
  serverVersion: state.main.serverVersion,
  currentSchema: state.tables.currentSchema,
  tableComment: state.tables.tableComment,
  columnComments: state.tables.columnComments,
  columnEdit: state.tables.modify.columnEdit,
  pkModify: state.tables.modify.pkModify,
  fkModify: state.tables.modify.fkModify,
  ...state.tables.modify,
});

const modifyTableConnector = connect => connect(mapStateToProps)(ModifyTable);

export default modifyTableConnector;
