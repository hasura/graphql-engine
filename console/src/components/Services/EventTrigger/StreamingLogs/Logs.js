import PropTypes from 'prop-types';
import React, { Component } from 'react';
import TableHeader from '../TableCommon/TableHeader';
import { loadEventLogs } from '../EventActions';

class StreamingLogs extends Component {
  componentDidMount() {
    this.props.dispatch(loadEventLogs(this.props.triggerName));
  }

  render() {
    const {
      triggerName,
      migrationMode,
      eventLogs,
      count,
      dispatch,
    } = this.props;

    console.log(eventLogs);
    const styles = require('../TableCommon/Table.scss');

    return (
      <div className={styles.container + ' container-fluid'}>
        <TableHeader
          count={count}
          dispatch={dispatch}
          triggerName={triggerName}
          tabName="logs"
          migrationMode={migrationMode}
        />
        <br />
        <div>Event logs streaming</div>
        <br />
        <br />
      </div>
    );
  }
}

StreamingLogs.propTypes = {
  eventLogs: PropTypes.array,
  migrationMode: PropTypes.bool.isRequired,
  dispatch: PropTypes.func.isRequired,
};

const mapStateToProps = (state, ownProps) => {
  return {
    ...state.triggers,
    triggerName: ownProps.params.trigger,
    migrationMode: state.main.migrationMode,
    currentSchema: state.tables.currentSchema,
  };
};

const streamingLogsConnector = connect =>
  connect(mapStateToProps)(StreamingLogs);

export default streamingLogsConnector;
