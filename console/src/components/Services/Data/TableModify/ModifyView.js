import PropTypes from 'prop-types';
import React, { Component } from 'react';
import AceEditor from 'react-ace';
import ViewHeader from '../TableBrowseRows/ViewHeader';
import {
  fetchViewDefinition,
  deleteViewSql,
  untrackTableSql,
  RESET,
} from './ModifyActions';
import TableCommentEditor from './TableCommentEditor';
import { ordinalColSort } from '../utils';
import { setTable } from '../DataActions';
import Button from '../../../Common/Button/Button';
import { NotFoundError } from '../../../Error/PageNotFound';

class ModifyView extends Component {
  componentDidMount() {
    const { dispatch } = this.props;
    dispatch({ type: RESET });
    dispatch(setTable(this.props.tableName));
    dispatch(fetchViewDefinition(this.props.tableName, false));
  }

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
      tableCommentEdit,
      migrationMode,
    } = this.props;

    const styles = require('./ModifyTable.scss');

    const tableSchema = allSchemas.find(
      t => t.table_name === tableName && t.table_schema === currentSchema
    );

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

    return (
      <div className={styles.container + ' container-fluid'}>
        <ViewHeader
          dispatch={dispatch}
          tableName={tableName}
          tabName="modify"
          currentSchema={currentSchema}
          migrationMode={migrationMode}
        />
        <br />
        <div className={'container-fluid ' + styles.padd_left_remove}>
          <div className={'col-xs-8 ' + styles.padd_left_remove}>
            <TableCommentEditor
              tableComment={tableComment}
              tableCommentEdit={tableCommentEdit}
              isTable={false}
              dispatch={dispatch}
            />
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
    migrationMode: state.main.migrationMode,
    serverVersion: state.main.serverVersion,
    ...state.tables.modify,
  };
};

const modifyViewConnector = connect => connect(mapStateToProps)(ModifyView);

export default modifyViewConnector;
