import PropTypes from 'prop-types';
import React, { Component } from 'react';
import { vSetDefaults, vMakeRequest, vExpandHeading } from './ViewActions'; // eslint-disable-line no-unused-vars
import { setTable } from '../DataActions';
import TableHeader from '../TableCommon/TableHeader';
import ViewHeader from './ViewHeader';
import ViewRows from './ViewRows';
import { replace } from 'react-router-redux';

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
    // Initialize this table
    this.state = {
      dispatch: props.dispatch,
      tableName: props.tableName,
    };
    // this.state.dispatch = props.dispatch;
    // this.state.tableName = props.tablename;
    const dispatch = this.props.dispatch;
    Promise.all([
      dispatch(setTable(this.props.tableName)),
      dispatch(vSetDefaults(this.props.tableName)),
      dispatch(vMakeRequest()),
    ]);
  }

  componentWillReceiveProps(nextProps) {
    const dispatch = this.props.dispatch;
    if (nextProps.tableName !== this.props.tableName) {
      dispatch(setTable(nextProps.tableName));
      dispatch(vSetDefaults(nextProps.tableName));
      dispatch(vMakeRequest());
    }
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

  render() {
    const {
      tableName,
      tableComment,
      schemas,
      query,
      curFilter,
      rows,
      count, // eslint-disable-line no-unused-vars
      activePath,
      migrationMode,
      ongoingRequest,
      isProgressing,
      lastError,
      lastSuccess,
      dispatch,
      expandedRow,
      currentSchema,
    } = this.props; // eslint-disable-line no-unused-vars

    // check if table exists
    const currentTable = schemas.find(s => s.table_name === tableName);
    if (!currentTable) {
      // dispatch a 404 route
      dispatch(replace('/404'));
    }
    // Is this a view
    const isView =
      schemas.find(s => s.table_name === tableName).detail.table_type !==
      'BASE TABLE';

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
    ...state.tables.view,
  };
};

const viewTableConnector = connect => connect(mapStateToProps)(ViewTable);

export default viewTableConnector;
