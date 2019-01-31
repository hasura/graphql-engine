import PropTypes from 'prop-types';
import React from 'react';
import TableHeader from '../TableCommon/TableHeader';
import {
  activateCommentEdit,
  updateCommentInput,
  saveTableCommentSql,
} from './ModifyActions';
import {
  fkLColChange,
  deleteTableSql,
  addColSql,
  untrackTableSql,
  RESET,
  TOGGLE_ACTIVE_COLUMN,
  saveColumnChangesSql,
  saveColChangesWithFkSql,
  deleteColumnSql,
} from '../TableModify/ModifyActions';
import { ordinalColSort } from '../utils';
import dataTypes from '../Common/DataTypes';
import { convertListToDict } from '../../../../utils/data';
import {
  setTable,
  fetchTableComment,
  fetchColumnComment,
} from '../DataActions';
import { showErrorNotification } from '../Notification';
import gqlPattern, { gqlColumnErrorNotif } from '../Common/GraphQLValidation';
import Button from '../../Layout/Button/Button';
import ColumnEditor from './ColumnEditor';
import semverCheck from '../../../../helpers/semver';

const alterTypeOptions = dataTypes.map((datatype, index) => (
  <option value={datatype.value} key={index} title={datatype.description}>
    {datatype.name}
  </option>
));

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
      activeEdit,
      fkAdd,
      migrationMode,
      currentSchema,
      tableComment,
      columnComment,
      tableCommentEdit,
    } = this.props;
    const styles = require('./Modify.scss');
    const tableSchema = allSchemas.find(t => t.table_name === tableName);
    const hasPrimaryKeys =
      tableSchema &&
      tableSchema.primary_key &&
      tableSchema.primary_key.columns.length > 0;
    const primaryKeyDict = hasPrimaryKeys
      ? convertListToDict(tableSchema.primary_key.columns)
      : {};

    const columns = tableSchema.columns.sort(ordinalColSort);
    const columnEditors = columns.map((c, i) => {
      let btnText = 'Edit';
      let colEditor = null;
      let bg = '';
      const colName = c.column_name;
      const onSubmit = (type, nullable, unique, def, comment, column) => {
        // dispatch(saveColumnChangesSql(tableName, colName, type, nullable, def, column));
        if (fkAdd.fkCheckBox === true) {
          dispatch(fkLColChange(column.column_name));
          dispatch(
            saveColChangesWithFkSql(
              tableName,
              colName,
              type,
              nullable,
              unique,
              def,
              comment,
              column
            )
          );
        } else {
          dispatch(
            saveColumnChangesSql(
              tableName,
              colName,
              type,
              nullable,
              unique,
              def,
              comment,
              column
            )
          );
        }
      };
      const onDelete = () => {
        const isOk = confirm('Are you sure you want to delete?');
        if (isOk) {
          dispatch(deleteColumnSql(tableName, colName, c));
        }
      };
      if (activeEdit.column === colName) {
        btnText = 'Close';
        if (primaryKeyDict[colName]) {
          colEditor = (
            <ColumnEditor
              column={c}
              onSubmit={onSubmit}
              onDelete={() => {
                const isOk = window.confirm(`Are you sure? Deleting a primary key DISABLE ALL ROW EDIT VIA THE CONSOLE.
              Also, this will delete everything associated with the column (included related entities in other tables) permanently?`);
                if (isOk) {
                  dispatch(deleteColumnSql(tableName, colName));
                }
              }}
              fkAdd={fkAdd}
              tableName={tableName}
              dispatch={dispatch}
              allSchemas={allSchemas}
              currentSchema={currentSchema}
              columnComment={columnComment}
              allowRename={this.state.supportTableColumnRename}
            />
          );
        } else {
          colEditor = (
            <ColumnEditor
              column={c}
              onSubmit={onSubmit}
              onDelete={onDelete}
              fkAdd={fkAdd}
              tableName={tableName}
              dispatch={dispatch}
              allSchemas={allSchemas}
              currentSchema={currentSchema}
              columnComment={columnComment}
              allowRename={this.state.supportTableColumnRename}
            />
          );
        }
        bg = styles.activeEdit;
      }
      // check if column name is primary key
      let isPrimaryKey = false;
      if (hasPrimaryKeys) {
        if (tableSchema.primary_key.columns.includes(colName)) {
          isPrimaryKey = true;
        }
      }
      let isForeignKey = false;
      if (tableSchema.foreign_key_constraints.length > 0) {
        const numFk = tableSchema.foreign_key_constraints.length;
        for (let iFk = 0; iFk < numFk; iFk++) {
          const fk = tableSchema.foreign_key_constraints[iFk];
          if (
            Object.keys(fk.column_mapping).toString() ===
            c.column_name.toString()
          ) {
            isForeignKey = true;
            break;
          }
        }
      }

      const keyProperties = () => {
        if (isPrimaryKey && isForeignKey) {
          return <i> - primary key, foreign key </i>;
        }
        if (isPrimaryKey) {
          return <i> - primary key </i>;
        }
        if (isForeignKey) {
          return <i> - foreign key </i>;
        }
        return <i> {''} </i>;
      };
      return (
        <div key={i} className={bg}>
          <div className="container-fluid">
            <div className="row">
              <h5 className={styles.padd_bottom}>
                <Button
                  className={styles.add_mar_small}
                  size="xs"
                  color="white"
                  data-test={`edit-${colName}`}
                  onClick={() => {
                    if (activeEdit.column === colName) {
                      // just closing the column
                      dispatch({ type: TOGGLE_ACTIVE_COLUMN, column: colName });
                    } else {
                      // fetch column comment
                      dispatch(fetchColumnComment(tableName, colName)).then(
                        () => {
                          dispatch({
                            type: TOGGLE_ACTIVE_COLUMN,
                            column: colName,
                          });
                        }
                      );
                    }
                  }}
                >
                  {btnText}
                </Button>
                <b>{colName}</b> {keyProperties()}
                &nbsp;
              </h5>
              {colEditor}
            </div>
          </div>
        </div>
      );
    });

    let colNameInput;
    let colTypeInput;
    let colNullInput;
    let colUniqueInput;
    let colDefaultInput;

    // TODO
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

    const editCommentClicked = () => {
      let commentText =
        tableComment && tableComment.result[1] && tableComment.result[1][0]
          ? tableComment.result[1][0]
          : null;
      commentText = commentText !== 'NULL' ? commentText : null;
      dispatch(activateCommentEdit(true, commentText));
    };
    const commentEdited = e => {
      dispatch(updateCommentInput(e.target.value));
    };
    const commentEditSave = () => {
      dispatch(saveTableCommentSql(true));
    };
    const commentEditCancel = () => {
      dispatch(activateCommentEdit(false, ''));
    };
    let commentText =
      tableComment && tableComment.result[1] && tableComment.result[1][0]
        ? tableComment.result[1][0]
        : null;
    commentText = commentText !== 'NULL' ? commentText : null;
    let commentHtml = (
      <div className={styles.add_pad_bottom}>
        <div className={styles.commentText}>Add a comment</div>
        <div onClick={editCommentClicked} className={styles.commentEdit}>
          <i className="fa fa-edit" />
        </div>
      </div>
    );
    if (commentText && !tableCommentEdit.enabled) {
      commentHtml = (
        <div className={styles.mar_bottom}>
          <div className={styles.commentText + ' alert alert-warning'}>
            {commentText}
          </div>
          <div onClick={editCommentClicked} className={styles.commentEdit}>
            <i className="fa fa-edit" />
          </div>
        </div>
      );
    } else if (tableCommentEdit.enabled) {
      commentHtml = (
        <div className={styles.mar_bottom}>
          <input
            onChange={commentEdited}
            className={'form-control ' + styles.commentInput}
            type="text"
            value={tableCommentEdit.value}
            defaultValue={commentText}
          />
          <div
            onClick={commentEditSave}
            className={
              styles.display_inline +
              ' ' +
              styles.add_pad_left +
              ' ' +
              styles.comment_action
            }
          >
            Save
          </div>
          <div
            onClick={commentEditCancel}
            className={
              styles.display_inline +
              ' ' +
              styles.add_pad_left +
              ' ' +
              styles.comment_action
            }
          >
            Cancel
          </div>
        </div>
      );
    }

    // if (tableSchema.primary_key.columns > 0) {}
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
              `col-xs-9 ${styles.padd_left_remove}` +
              ' ' +
              styles.modifyMinWidth
            }
          >
            {commentHtml}
            <h4 className={styles.subheading_text}>Columns</h4>
            {columnEditors}
            <hr />
            <h4 className={styles.subheading_text}>Add a new column</h4>
            <div className={styles.activeEdit}>
              <form
                className={`form-inline ${styles.display_flex}`}
                onSubmit={e => {
                  e.preventDefault();
                  // validate before sending
                  if (!gqlPattern.test(colNameInput.value)) {
                    dispatch(
                      showErrorNotification(
                        gqlColumnErrorNotif[0],
                        gqlColumnErrorNotif[1],
                        gqlColumnErrorNotif[2],
                        gqlColumnErrorNotif[3]
                      )
                    );
                  } else if (
                    colNameInput.value === '' ||
                    colTypeInput.value === ''
                  ) {
                    dispatch(
                      showErrorNotification(
                        'Error creating column!',
                        'Column name/type cannot be empty',
                        '',
                        {
                          custom: 'Column name/type cannot be empty',
                        }
                      )
                    );
                  } else {
                    dispatch(
                      addColSql(
                        tableName,
                        colNameInput.value,
                        colTypeInput.value,
                        colNullInput.checked,
                        colUniqueInput.checked,
                        colDefaultInput.value,
                        e.target
                      )
                    );
                  }
                }}
              >
                <input
                  placeholder="column name"
                  type="text"
                  className={`${styles.input} input-sm form-control`}
                  ref={n => (colNameInput = n)}
                  data-test="column-name"
                />
                <select
                  className={`${styles.select} input-sm form-control`}
                  defaultValue=""
                  ref={n => (colTypeInput = n)}
                  data-test="data-type"
                >
                  <option disabled value="">
                    -- type --
                  </option>
                  {alterTypeOptions}
                </select>
                <input
                  type="checkbox"
                  defaultChecked
                  className={`${styles.input} ${
                    styles.nullable
                  } input-sm form-control`}
                  ref={n => (colNullInput = n)}
                  data-test="nullable-checkbox"
                />
                <label className={styles.nullLabel}>Nullable</label>
                <input
                  type="checkbox"
                  className={`${styles.input} ${
                    styles.nullable
                  } input-sm form-control`}
                  ref={n => (colUniqueInput = n)}
                  data-test="unique-checkbox"
                />
                <label className={styles.nullLabel}>Unique</label>
                <input
                  placeholder="default value"
                  type="text"
                  className={`${styles.input} ${
                    styles.defaultInput
                  } input-sm form-control`}
                  ref={n => (colDefaultInput = n)}
                  data-test="default-value"
                />
                <Button
                  type="submit"
                  color="yellow"
                  size="sm"
                  data-test="add-column-button"
                >
                  + Add column
                </Button>
              </form>
            </div>
            <hr />
            {untrackBtn}
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
  columnComment: PropTypes.string.isRequired,
  activeEdit: PropTypes.object.isRequired,
  fkAdd: PropTypes.object.isRequired,
  relAdd: PropTypes.object.isRequired,
  ongoingRequest: PropTypes.bool.isRequired,
  lastError: PropTypes.object,
  lastFormError: PropTypes.object,
  lastSuccess: PropTypes.bool,
  dispatch: PropTypes.func.isRequired,
  serverVersion: PropTypes.string,
};

const mapStateToProps = (state, ownProps) => ({
  tableName: ownProps.params.table,
  allSchemas: state.tables.allSchemas,
  migrationMode: state.main.migrationMode,
  serverVersion: state.main.serverVersion,
  currentSchema: state.tables.currentSchema,
  tableComment: state.tables.tableComment,
  columnComment: state.tables.columnComment,
  ...state.tables.modify,
});

const modifyTableConnector = connect => connect(mapStateToProps)(ModifyTable);

export default modifyTableConnector;
