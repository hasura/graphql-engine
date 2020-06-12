import PropTypes from 'prop-types';
import React, { Component } from 'react';
import {
  vSetDefaults,
  // vExpandHeading,
  fetchManualTriggers,
  UPDATE_TRIGGER_ROW,
  UPDATE_TRIGGER_FUNCTION,
  vMakeTableRequests,
} from './ViewActions';
import { checkIfTable } from '../../../Common/utils/pgUtils';
import { setTable } from '../DataActions';
import TableHeader from '../TableCommon/TableHeader';
import ViewRows from './ViewRows';

import { NotFoundError } from '../../../Error/PageNotFound';
import { exists } from '../../../Common/utils/jsUtils';
import { getPersistedPageSize } from './localStorageUtils';

class ViewTable extends Component {
  constructor(props) {
    super(props);

    this.state = {
      dispatch: props.dispatch,
      tableName: props.tableName,
    };

    this.getInitialData(this.props.tableName);
  }

  UNSAFE_componentWillReceiveProps(nextProps) {
    if (nextProps.tableName !== this.props.tableName) {
      this.getInitialData(nextProps.tableName);
    }
  }

  getInitialData(tableName) {
    const { dispatch, currentSchema } = this.props;
    const limit = getPersistedPageSize(tableName, currentSchema);
    Promise.all([
      dispatch(setTable(tableName)),
      dispatch(vSetDefaults(limit)),
      dispatch(vMakeTableRequests()),
      dispatch(fetchManualTriggers(tableName)),
    ]);
  }

  shouldComponentUpdate(nextProps) {
    return (
      this.props.tableName === null ||
      nextProps.tableName === this.props.tableName
    );
  }

  componentWillUpdate() {
    this.shouldScrollBottom =
      window.innerHeight ===
      document.body.offsetHeight - document.body.scrollTop;
  }

  componentDidUpdate() {
    if (this.shouldScrollBottom) {
      document.body.scrollTop = document.body.offsetHeight - window.innerHeight;
    }
  }

  componentWillUnmount() {
    // Remove state data beloging to this table
    const dispatch = this.props.dispatch;
    dispatch(vSetDefaults());
  }

  updateInvocationRow = row => {
    const { dispatch } = this.props;
    dispatch({
      type: UPDATE_TRIGGER_ROW,
      data: row,
    });
  };

  updateInvocationFunction = triggerFunc => {
    const { dispatch } = this.props;
    dispatch({
      type: UPDATE_TRIGGER_FUNCTION,
      data: triggerFunc,
    });
  };

  render() {
    const {
      tableName,
      schemas,
      query,
      curFilter,
      rows,
      count,
      activePath,
      migrationMode,
      readOnlyMode,
      ongoingRequest,
      isProgressing,
      lastError,
      lastSuccess,
      dispatch,
      expandedRow,
      currentSchema,
      manualTriggers = [],
      triggeredRow,
      triggeredFunction,
      location,
      estimatedCount,
      isCountEstimated,
    } = this.props;

    // check if table exists
    const tableSchema = schemas.find(
      s => s.table_name === tableName && s.table_schema === currentSchema
    );

    if (!tableSchema) {
      // throw a 404 exception
      throw new NotFoundError();
    }

    const styles = require('../../../Common/Common.scss');

    // Is this a view
    const isView = !checkIfTable(tableSchema);

    // Are there any expanded columns
    const viewRows = (
      <ViewRows
        curTableName={tableName}
        currentSchema={currentSchema}
        curQuery={query}
        curFilter={curFilter}
        curPath={[]}
        curRows={rows}
        isView={isView}
        parentTableName={null}
        activePath={activePath}
        ongoingRequest={ongoingRequest}
        isProgressing={isProgressing}
        lastError={lastError}
        lastSuccess={lastSuccess}
        schemas={schemas}
        curDepth={0}
        count={exists(count) ? count : estimatedCount}
        dispatch={dispatch}
        expandedRow={expandedRow}
        manualTriggers={manualTriggers}
        updateInvocationRow={this.updateInvocationRow.bind(this)}
        updateInvocationFunction={this.updateInvocationFunction.bind(this)}
        triggeredRow={triggeredRow}
        triggeredFunction={triggeredFunction}
        location={location}
        readOnlyMode={readOnlyMode}
      />
    );

    // Choose the right nav bar header thing
    const header = (
      <TableHeader
        count={isCountEstimated ? estimatedCount : count}
        isCountEstimated={isCountEstimated}
        dispatch={dispatch}
        table={tableSchema}
        tabName="browse"
        migrationMode={migrationMode}
        readOnlyMode={readOnlyMode}
      />
    );

    let comment = null;
    if (tableSchema.comment) {
      comment = (
        <div className={styles.add_mar_top}>
          <div className={styles.commentText + ' alert alert-warning'}>
            {tableSchema.comment}
          </div>
        </div>
      );
    }

    return (
      <div>
        {header}
        {comment}
        <div>{viewRows}</div>
      </div>
    );
  }
}

ViewTable.propTypes = {
  tableName: PropTypes.string.isRequired,
  tableComment: PropTypes.object,
  schemas: PropTypes.array.isRequired,
  currentSchema: PropTypes.string.isRequired,
  activePath: PropTypes.array.isRequired,
  query: PropTypes.object.isRequired,
  curFilter: PropTypes.object.isRequired,
  migrationMode: PropTypes.bool.isRequired,
  readOnlyMode: PropTypes.bool.isRequired,
  ongoingRequest: PropTypes.bool.isRequired,
  isProgressing: PropTypes.bool.isRequired,
  rows: PropTypes.array.isRequired,
  expandedRow: PropTypes.string.isRequired,
  count: PropTypes.number,
  lastError: PropTypes.object.isRequired,
  lastSuccess: PropTypes.object.isRequired,
  dispatch: PropTypes.func.isRequired,
};

const mapStateToProps = (state, ownProps) => {
  return {
    tableName: ownProps.params.table,
    currentSchema: state.tables.currentSchema,
    schemas: state.tables.allSchemas,
    tableComment: state.tables.tableComment,
    migrationMode: state.main.migrationMode,
    readOnlyMode: state.main.readOnlyMode,
    serverVersion: state.main.serverVersion,
    ...state.tables.view,
  };
};

const viewTableConnector = connect => connect(mapStateToProps)(ViewTable);

export default viewTableConnector;
