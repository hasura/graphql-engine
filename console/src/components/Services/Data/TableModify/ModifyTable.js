import PropTypes from 'prop-types';
import React from 'react';
import TableHeader from '../TableCommon/TableHeader';
import {
  activateCommentEdit,
  updateCommentInput,
  saveTableCommentSql,
} from './ModifyActions';
import {
  deleteTableSql,
  untrackTableSql,
  RESET,
  saveColumnChangesSql,
  deleteColumnSql,
  setColumnEdit,
  resetColumnEdit,
  addPrimaryKey,
  removePrimaryKey,
  resetPrimaryKeys,
  savePrimaryKeys,
} from '../TableModify/ModifyActions';
import { ordinalColSort } from '../utils';
import {
  setTable,
  fetchTableComment,
  fetchColumnComment,
} from '../DataActions';
import Button from '../../../Common/Button/Button';
import PrimaryKeySelector from '../Common/ReusableComponents/PrimaryKeySelector';
import ColumnEditor from './ColumnEditor';
import ColumnCreator from './ColumnCreator';
import semverCheck from '../../../../helpers/semver';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';

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
      columnComment,
      tableCommentEdit,
      columnEdit,
      pkModify,
    } = this.props;
    const styles = require('./ModifyTable.scss');
    const tableSchema = allSchemas.find(t => t.table_name === tableName);
    const columns = tableSchema.columns.sort(ordinalColSort);
    const tablePrimaryKeyColumns = tableSchema.primary_key
      ? tableSchema.primary_key.columns
      : [];
    const columnEditors = columns.map((c, i) => {
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
      for (
        let uci = tableSchema.unique_constraints.length - 1;
        uci >= 0;
        uci--
      ) {
        const constraint = tableSchema.unique_constraints[uci];
        if (
          constraint.columns.length === 1 &&
          constraint.columns[0] === c.column_name
        ) {
          columnProperties.isUnique = true;
        }
      }
      const onSubmit = () => {
        dispatch(saveColumnChangesSql(colName, c));
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
          confirmMessage = `Are you sure? Deleting a primary key DISABLE ALL ROW EDIT VIA THE CONSOLE.
        Also, this will delete everything associated with the column (included related entities in other tables) permanently?`;
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
          <div key={i}>
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
          <div key={i}>
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
          <div key={i}>
            <ColumnEditor
              column={c}
              onSubmit={onSubmit}
              onDelete={safeOnDelete}
              tableName={tableName}
              dispatch={dispatch}
              allSchemas={allSchemas}
              currentSchema={currentSchema}
              columnComment={columnComment}
              allowRename={this.state.supportTableColumnRename}
              columnProperties={columnProperties}
              columnEdit={columnEdit}
            />
          </div>
        );
      };
      return (
        <ExpandableEditor
          editorExpanded={colEditorExpanded}
          property={'edit-column'}
          ongoingRequest={'oola'}
          service="modify-table"
          saveFunc={onSubmit}
          removeFunc={columnProperties.isPrimaryKey ? null : onDelete}
          collapsedClass={styles.display_flex}
          expandedLabel={expandedLabel}
          collapsedLabel={collapsedLabel}
          expandCallback={() => {
            dispatch(setColumnEdit(columnProperties));
            dispatch(fetchColumnComment(tableName, colName));
          }}
          collapseCallback={() => {
            dispatch(resetColumnEdit(colName));
          }}
        />
      );
    });

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

    const primaryKeyEditors = () => {
      const orderedCols = columns.map((c, _i) => ({
        name: c.column_name,
        type: c.data_type,
        index: _i,
      }));
      const orderedPks = tablePrimaryKeyColumns.map(pk => {
        return orderedCols.find(c => c.name === pk).index;
      });
      const tempPks = pkModify.filter(pkm => pkm !== '').map(pkm => {
        return orderedCols[pkm].name;
      });

      const pkConstraintName = tableSchema.primary_key
        ? tableSchema.primary_key.constraint_name
        : '';
      const pkConfigText = tablePrimaryKeyColumns.join(', ');
      const pkEditorCollapsedLabel = () => (
        <div>
          <div className="container-fluid">
            <div className="row">
              <h5 className={styles.padd_bottom}>
                {pkConfigText ? <i> ({pkConfigText}) </i> : 'No primary key'}
                &nbsp;
              </h5>
            </div>
          </div>
        </div>
      );
      const pkEditorExpandedLabel = () => (
        <h5 className={styles.padd_bottom}>
          <b> {pkConfigText && `(${pkConfigText})`}</b>
        </h5>
      );
      const pkEditorExpanded = () => (
        <div>
          <div className={`container-fluid ${styles.pkEditorExpandedText}`}>
            <div className="row">
              <h5 className={styles.padd_bottom}>
                Selected configuration: <i>{`( ${tempPks.join(', ')} )`}</i>
                &nbsp;
              </h5>
            </div>
          </div>
          <div className={`${styles.pkEditorExpanded}`}>
            <PrimaryKeySelector
              dispatch={dispatch}
              setPk={addPrimaryKey}
              removePk={removePrimaryKey}
              columns={orderedCols}
              primaryKeys={pkModify}
              service="modify-table"
            />
          </div>
        </div>
      );

      const onSave = () => {
        dispatch(savePrimaryKeys(tableName, currentSchema, pkConstraintName));
      };

      const onRemove = () => {
        dispatch(resetPrimaryKeys());
        onSave();
      };

      /*
        TODO
        Handle cases when there is no primary keys
        Handle primary key removal
      */

      return (
        <ExpandableEditor
          collapsedLabel={pkEditorCollapsedLabel}
          expandedLabel={pkEditorExpandedLabel}
          editorExpanded={pkEditorExpanded}
          property={'edit-pks'}
          ongoingRequest={'todo'}
          service="modify-table"
          saveFunc={onSave}
          removeFunc={onRemove}
          expandCallback={() => {
            orderedPks.forEach((oPk, __i) => {
              dispatch(addPrimaryKey(oPk.toString(), __i));
            });
          }}
          collapseCallback={() => {
            dispatch(resetPrimaryKeys());
          }}
        />
      );
    };
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
              `col-xs-10 ${styles.padd_left_remove}` +
              ' ' +
              styles.modifyMinWidth
            }
          >
            {commentHtml}
            <h4 className={styles.subheading_text}>Columns</h4>
            {columnEditors}
            <hr />
            <h4 className={styles.subheading_text}>Add a new column</h4>
            <ColumnCreator
              styles={styles}
              dispatch={dispatch}
              tableName={tableName}
            />
            <hr />
            <h4 className={styles.subheading_text}>Primary keys</h4>
            {primaryKeyEditors()}
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
  columnEdit: PropTypes.object.isRequired,
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
  columnEdit: state.tables.modify.columnEdit,
  pkModify: state.tables.modify.pkModify,
  ...state.tables.modify,
});

const modifyTableConnector = connect => connect(mapStateToProps)(ModifyTable);

export default modifyTableConnector;
