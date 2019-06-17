import PropTypes from 'prop-types';
import React, { Component } from 'react';
import { vSetDefaults, vMakeRequest, vExpandHeading } from './ViewActions'; // eslint-disable-line no-unused-vars
import { setTrigger } from '../EventActions';
import TableHeader from '../TableCommon/TableHeader';
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
    // Initialize this table
    this.state = {
      dispatch: props.dispatch,
      triggerName: props.triggerName,
    };
    // this.state.dispatch = props.dispatch;
    // this.state.triggerName = props.triggerName;
    const dispatch = this.props.dispatch;
    Promise.all([
      dispatch(setTrigger(this.props.triggerName)),
      dispatch(vSetDefaults(this.props.triggerName)),
      dispatch(vMakeRequest()),
    ]);
  }

  componentWillReceiveProps(nextProps) {
    const dispatch = this.props.dispatch;
    if (nextProps.triggerName !== this.props.triggerName) {
      dispatch(setTrigger(nextProps.triggerName));
      dispatch(vSetDefaults(nextProps.triggerName));
      dispatch(vMakeRequest());
    }
  }

  shouldComponentUpdate(nextProps) {
    return (
      this.props.triggerName === null ||
      nextProps.triggerName === this.props.triggerName
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
    dispatch(vSetDefaults(this.props.triggerName));
  }

  render() {
    const {
      triggerName,
      triggerList,
      query,
      curFilter,
      rows,
      count, // eslint-disable-line no-unused-vars
      activePath,
      ongoingRequest,
      isProgressing,
      lastError,
      lastSuccess,
      dispatch,
      expandedRow,
    } = this.props; // eslint-disable-line no-unused-vars

    // check if table exists
    const currentTrigger = triggerList.find(s => s.name === triggerName);
    if (!currentTrigger) {
      // throw a 404 exception
      throw new NotFoundError();
    }
    // Is this a view
    const isView = false;

    // Are there any expanded columns
    const viewRows = (
      <ViewRows
        curTriggerName={triggerName}
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
        triggerList={triggerList}
        curDepth={0}
        count={count}
        dispatch={dispatch}
        expandedRow={expandedRow}
      />
    );

    // Choose the right nav bar header thing
    const header = (
      <TableHeader
        count={count}
        dispatch={dispatch}
        triggerName={triggerName}
        tabName="running"
      />
    );

    return (
      <div>
        {header}
        <div>{viewRows}</div>
      </div>
    );
  }
}

ViewTable.propTypes = {
  triggerName: PropTypes.string.isRequired,
  triggerList: PropTypes.array.isRequired,
  activePath: PropTypes.array.isRequired,
  query: PropTypes.object.isRequired,
  curFilter: PropTypes.object.isRequired,
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
    triggerName: ownProps.params.trigger,
    triggerList: state.triggers.runningEvents,
    ...state.triggers.view,
  };
};

const runningEventsConnector = connect => connect(mapStateToProps)(ViewTable);

export default runningEventsConnector;
