import PropTypes from 'prop-types';
import React, { Component } from 'react';
import {
  vSetDefaults,
  vMakeRequest,
  // vExpandHeading,
  fetchManualTriggers,
  UPDATE_TRIGGER_ROW,
  UPDATE_TRIGGER_FUNCTION,
} from './ViewActions';
import { setTable } from '../DataActions';
import TableHeader from '../TableCommon/TableHeader';
import ViewHeader from './ViewHeader';
import ViewRows from './ViewRows';

import { NotFoundError } from '../../../Error/PageNotFound';

const genHeadings = headings => {
  if (headings.length === 0) {
    return [];
  }

  const heading = headings[0];
  if (typeof heading === 'string') {
    return [heading, ...genHeadings(headings.slice(1))];
  }
  if (typeof heading === 'object') {
    if (!heading._expanded) {
      const headingName =
        heading.type === 'obj_rel' ? heading.lcol : heading.relname;
      return [
        { name: headingName, type: heading.type },
        ...genHeadings(headings.slice(1)),
      ];
    }
    if (heading.type === 'obj_rel') {
      const subheadings = genHeadings(heading.headings).map(h => {
        if (typeof h === 'string') {
          return heading.relname + '.' + h;
        }
        return heading.relname + '.' + h.name;
      });
      return [...subheadings, ...genHeadings(headings.slice(1))];
    }
  }

  throw 'Incomplete pattern match'; // eslint-disable-line no-throw-literal
};

const genRow = (row, headings) => {
  if (headings.length === 0) {
    return [];
  }

  const heading = headings[0];
  if (typeof heading === 'string') {
    return [row[heading], ...genRow(row, headings.slice(1))];
  }
  if (typeof heading === 'object') {
    if (!heading._expanded) {
      const rowVal = heading.type === 'obj_rel' ? row[heading.lcol] : '[...]';
      return [rowVal, ...genRow(row, headings.slice(1))];
    }
    if (heading.type === 'obj_rel') {
      const subrow = genRow(row[heading.relname], heading.headings);
      return [...subrow, ...genRow(row, headings.slice(1))];
    }
  }

  throw 'Incomplete pattern match'; // eslint-disable-line no-throw-literal
};

class ViewTable extends Component {
  constructor(props) {
    super(props);

    this.state = {
      dispatch: props.dispatch,
      tableName: props.tableName,
    };

    this.getInitialData(this.props.tableName);
  }

  componentWillReceiveProps(nextProps) {
    if (nextProps.tableName !== this.props.tableName) {
      this.getInitialData(nextProps.tableName);
    }
  }

  getInitialData(tableName) {
    const { dispatch } = this.props;
    Promise.all([
      dispatch(setTable(tableName)),
      dispatch(vSetDefaults(tableName)),
      dispatch(vMakeRequest()),
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
    dispatch(vSetDefaults(this.props.tableName));
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
      tableComment,
      schemas,
      query,
      curFilter,
      rows,
      count,
      activePath,
      migrationMode,
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
    } = this.props;

    // check if table exists
    const currentTable = schemas.find(
      s => s.table_name === tableName && s.table_schema === currentSchema
    );

    if (!currentTable) {
      // throw a 404 exception
      throw new NotFoundError();
    }

    // Is this a view
    const isView = currentTable.table_type !== 'BASE TABLE';

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
        count={count}
        dispatch={dispatch}
        expandedRow={expandedRow}
        manualTriggers={manualTriggers}
        updateInvocationRow={this.updateInvocationRow.bind(this)}
        updateInvocationFunction={this.updateInvocationFunction.bind(this)}
        triggeredRow={triggeredRow}
        triggeredFunction={triggeredFunction}
      />
    );

    // Choose the right nav bar header thing
    let header = (
      <TableHeader
        count={count}
        dispatch={dispatch}
        tableName={tableName}
        tableComment={tableComment}
        tabName="browse"
        migrationMode={migrationMode}
        currentSchema={currentSchema}
      />
    );
    if (isView) {
      header = (
        <ViewHeader
          dispatch={dispatch}
          tableName={tableName}
          tabName="browse"
          tableComment={tableComment}
          migrationMode={migrationMode}
          currentSchema={currentSchema}
        />
      );
    }

    return (
      <div>
        {header}
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
    serverVersion: state.main.serverVersion,
    ...state.tables.view,
  };
};

const viewTableConnector = connect => connect(mapStateToProps)(ViewTable);

export default viewTableConnector;
