import PropTypes from 'prop-types';
import React, { Component } from 'react';
import AceEditor from 'react-ace';
import ViewHeader from '../TableBrowseRows/ViewHeader';
import {
  activateCommentEdit,
  updateCommentInput,
  saveTableCommentSql,
} from './ModifyActions';
import {
  fetchViewDefinition,
  deleteViewSql,
  untrackTableSql,
  RESET,
} from './ModifyActions';
import { ordinalColSort } from '../utils';
import { setTable, fetchTableComment } from '../DataActions';
import Button from '../../../Common/Button/Button';
import semverCheck from '../../../../helpers/semver';

class ModifyView extends Component {
  state = {
    supportTableColumnRename: false,
  };
  componentDidMount() {
    const { dispatch, serverVersion } = this.props;
    dispatch({ type: RESET });
    dispatch(setTable(this.props.tableName));
    dispatch(fetchViewDefinition(this.props.tableName, false));
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

  modifyViewDefinition = viewName => {
    // fetch the definition
    this.props.dispatch(fetchViewDefinition(viewName, true));
    // redirect the user to run_sql page and set state
  };

  render() {
    const {
      sql,
      tableName,
      allSchemas,
      ongoingRequest,
      lastError,
      lastSuccess,
      dispatch,
      currentSchema,
      tableComment,
      tableCommentEdit,
      migrationMode,
    } = this.props;

    const styles = require('./ModifyTable.scss');

    const tableSchema = allSchemas.find(t => t.table_name === tableName); // eslint-disable-line no-unused-vars

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

    const columns = tableSchema.columns.sort(ordinalColSort);
    const columnEditors = columns.map((c, i) => {
      const bg = '';
      return (
        <div key={i} className={bg}>
          <div className="container-fluid">
            <div className={`row + ${styles.add_mar_bottom}`}>
              <h5>
                <Button disabled="disabled" size="xs">
                  -
                </Button>{' '}
                &nbsp; <b>{c.column_name}</b>
              </h5>
            </div>
          </div>
        </div>
      );
    });

    const modifyBtn = (
      <Button
        type="submit"
        color="yellow"
        size="sm"
        className={styles.add_mar_right}
        onClick={() => {
          this.modifyViewDefinition(tableName);
        }}
        data-test="modify-view"
      >
        Modify
      </Button>
    );

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
        data-test="untrack-view"
      >
        Untrack View
      </Button>
    );

    const deleteBtn = (
      <Button
        type="submit"
        color="red"
        size="sm"
        onClick={() => {
          const isOk = confirm('Are you sure');
          if (isOk) {
            dispatch(deleteViewSql(tableName));
          }
        }}
        data-test="delete-view"
      >
        Delete view
      </Button>
    );

    const editCommentClicked = () => {
      dispatch(activateCommentEdit(true, tableComment));
    };
    const commentEdited = e => {
      dispatch(updateCommentInput(e.target.value));
    };
    const commentEditSave = () => {
      dispatch(saveTableCommentSql(false));
    };
    const commentEditCancel = () => {
      dispatch(activateCommentEdit(false, null));
    };
    const commentText = tableComment ? tableComment.result[1] : null;
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
        <div>
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
            defaultValue={tableComment.result[1]}
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

    return (
      <div className={styles.container + ' container-fluid'}>
        <ViewHeader
          dispatch={dispatch}
          tableName={tableName}
          tabName="modify"
          currentSchema={currentSchema}
          migrationMode={migrationMode}
          allowRename={this.state.supportTableColumnRename}
        />
        <br />
        <div className={'container-fluid ' + styles.padd_left_remove}>
          <div className={'col-xs-8 ' + styles.padd_left_remove}>
            {commentHtml}
            <h4 className={styles.subheading_text}>Columns</h4>
            {columnEditors}
            <br />
            <h4>View Definition:</h4>
            <AceEditor
              mode="sql"
              theme="github"
              value={sql}
              name="raw_sql"
              minLines={8}
              maxLines={100}
              width="100%"
              showPrintMargin={false}
              readOnly
            />
            <hr />
            {modifyBtn}
            {untrackBtn}
            {deleteBtn}
            <br />
            <br />
          </div>
          <div className={styles.fixed + ' col-xs-3 hidden'}>{alert}</div>
        </div>
      </div>
    );
  }
}

ModifyView.propTypes = {
  sql: PropTypes.string.isRequired,
  tableName: PropTypes.string.isRequired,
  allSchemas: PropTypes.array.isRequired,
  currentSchema: PropTypes.string.isRequired,
  tableComment: PropTypes.string.isRequired,
  activeEdit: PropTypes.object.isRequired,
  ongoingRequest: PropTypes.bool.isRequired,
  lastError: PropTypes.object,
  lastSuccess: PropTypes.bool,
  dispatch: PropTypes.func.isRequired,
  serverVersion: PropTypes.string,
};

const mapStateToProps = (state, ownProps) => {
  return {
    tableName: ownProps.params.table,
    allSchemas: state.tables.allSchemas,
    sql: state.rawSQL.sql,
    currentSchema: state.tables.currentSchema,
    tableComment: state.tables.tableComment,
    migrationMode: state.main.migrationMode,
    serverVersion: state.main.serverVersion,
    ...state.tables.modify,
  };
};

const modifyViewConnector = connect => connect(mapStateToProps)(ModifyView);

export default modifyViewConnector;
