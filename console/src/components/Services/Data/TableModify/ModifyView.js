import PropTypes from 'prop-types';
import React, { Component } from 'react';
import AceEditor from 'react-ace';
import ViewHeader from '../TableBrowseRows/ViewHeader';
import {
  fetchViewDefinition,
  deleteViewSql,
  untrackTableSql,
  RESET,
} from '../TableModify/ModifyActions';
import { ordinalColSort } from '../utils';
import { setTable } from '../DataActions';

class ModifyView extends Component {
  componentDidMount() {
    this.props.dispatch({ type: RESET });

    this.props.dispatch(setTable(this.props.tableName));
    this.props.dispatch(fetchViewDefinition(this.props.tableName, false));
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
    } = this.props;

    const styles = require('./Modify.scss');

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
      const btnText = '-';
      const bg = '';
      return (
        <div key={i} className={bg}>
          <div className="container-fluid">
            <div className="row">
              <h5 className={styles.padd_bottom}>
                <button disabled="disabled" className="btn btn-xs btn-warning">
                  {btnText}
                </button>{' '}
                &nbsp; {c.column_name}
              </h5>
            </div>
          </div>
        </div>
      );
    });

    const untrackBtn = (
      <button
        type="submit"
        className={styles.add_mar_right + ' btn btn-sm btn-default'}
        onClick={() => {
          const isOk = confirm('Are you sure to untrack?');
          if (isOk) {
            dispatch(untrackTableSql(tableName));
          }
        }}
        data-test="untrack-view"
      >
        Untrack View
      </button>
    );

    return (
      <div className={styles.container + ' container-fluid'}>
        <ViewHeader
          dispatch={dispatch}
          tableName={tableName}
          tabName="modify"
          currentSchema={currentSchema}
        />
        <br />
        <div className={'container-fluid ' + styles.padd_left_remove}>
          <div className={'col-xs-8 ' + styles.padd_left_remove}>
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
            <button
              type="submit"
              className={
                'btn btn-sm ' +
                styles.yellow_button +
                ' ' +
                styles.add_mar_right
              }
              onClick={() => {
                this.modifyViewDefinition(tableName);
              }}
              data-test="modify-view"
            >
              Modify
            </button>
            {untrackBtn}
            <button
              type="submit"
              className={'btn btn-sm btn-danger'}
              onClick={() => {
                const isOk = confirm('Are you sure');
                if (isOk) {
                  dispatch(deleteViewSql(tableName));
                }
              }}
              data-test="delete-view"
            >
              Delete view
            </button>
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
};

const mapStateToProps = (state, ownProps) => {
  return {
    tableName: ownProps.params.table,
    allSchemas: state.tables.allSchemas,
    sql: state.rawSQL.sql,
    currentSchema: state.tables.currentSchema,
    ...state.tables.modify,
  };
};

const modifyViewConnector = connect => connect(mapStateToProps)(ModifyView);

export default modifyViewConnector;
