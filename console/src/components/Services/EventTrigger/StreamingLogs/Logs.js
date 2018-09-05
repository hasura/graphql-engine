import PropTypes from 'prop-types';
import React, { Component } from 'react';
import ReactTable from 'react-table';
import AceEditor from 'react-ace';
import Tabs from 'react-bootstrap/lib/Tabs';
import Tab from 'react-bootstrap/lib/Tab';
import TableHeader from '../TableCommon/TableHeader';
import { loadEventLogs, setTrigger } from '../EventActions';
import {
  vMakeRequest,
  vSetDefaults,
  loadNewerEvents,
  loadOlderEvents,
} from './LogActions';

class StreamingLogs extends Component {
  constructor(props) {
    super(props);
    this.state = { isWatching: false, intervalId: null };
    this.refreshData = this.refreshData.bind(this);
    this.props.dispatch(setTrigger(this.props.triggerName));
  }
  componentDidMount() {
    this.props.dispatch(setTrigger(this.props.triggerName));
    this.props.dispatch(loadEventLogs(this.props.triggerName));
  }
  componentWillUnmount() {
    this.props.dispatch(vSetDefaults());
  }
  handleNewerEvents() {
    // get the first element
    const firstElement = this.props.log.rows[0];
    const latestTimestamp = firstElement.created_at;
    this.props.dispatch(
      loadNewerEvents(latestTimestamp, this.props.triggerName)
    );
  }
  handleOlderEvents() {
    // get the last element
    const lastElement = this.props.log.rows[this.props.log.rows.length - 1];
    const oldestTimestamp = lastElement.created_at;
    this.props.dispatch(
      loadOlderEvents(oldestTimestamp, this.props.triggerName)
    );
  }
  watchChanges() {
    // set state on watch
    this.setState({ isWatching: !this.state.isWatching });
    if (this.state.isWatching) {
      clearInterval(this.state.intervalId);
    } else {
      const intervalId = setInterval(this.refreshData, 2000);
      this.setState({ intervalId: intervalId });
    }
  }
  refreshData() {
    this.props.dispatch(vMakeRequest(this.props.triggerName));
  }

  render() {
    const { triggerName, migrationMode, log, count, dispatch } = this.props;

    const styles = require('../TableCommon/Table.scss');
    const invocationColumns = [
      'status',
      'invocation_id',
      'event_id',
      'created_at',
    ];
    const invocationGridHeadings = [];
    invocationColumns.map(column => {
      invocationGridHeadings.push({
        Header: column,
        accessor: column,
      });
    });
    const invocationRowsData = [];
    log.rows.map(r => {
      const newRow = {};
      const status =
        r.status === 200 ? (
          <i className={styles.invocationSuccess + ' fa fa-check'} />
        ) : (
          <i className={styles.invocationFailure + ' fa fa-times'} />
        );

      // Insert cells corresponding to all rows
      invocationColumns.forEach(col => {
        const getCellContent = () => {
          const conditionalClassname = styles.tableCellCenterAligned;
          if (r[col] === null) {
            return (
              <div className={conditionalClassname}>
                <i>NULL</i>
              </div>
            );
          }
          if (col === 'status') {
            return <div className={conditionalClassname}>{status}</div>;
          }
          if (col === 'invocation_id') {
            return <div className={conditionalClassname}>{r.id}</div>;
          }
          if (col === 'created_at') {
            const formattedDate = new Date(r.created_at).toUTCString();
            return <div className={conditionalClassname}>{formattedDate}</div>;
          }
          const content = r[col] === undefined ? 'NULL' : r[col].toString();
          return <div className={conditionalClassname}>{content}</div>;
        };
        newRow[col] = getCellContent();
      });
      invocationRowsData.push(newRow);
    });

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
        <div>
          <button
            onClick={this.watchChanges.bind(this)}
            className={styles.watchBtn + ' btn btn-default'}
            data-test="run-query"
          >
            {this.state.isWatching ? (
              <span>
                <i className={'fa fa-pause'} /> Streaming...{' '}
                <i className={'fa fa-spinner fa-spin'} />
              </span>
            ) : (
              <span>
                Stream Logs <i className={'fa fa-play'} />
              </span>
            )}
          </button>
        </div>
        {invocationRowsData.length ? (
          <div className={styles.streamingLogs + ' streamingLogs'}>
            <div className={styles.loadNewer}>
              <button
                onClick={this.handleNewerEvents.bind(this)}
                className={'btn btn-default'}
              >
                {' '}
                Load newer events
              </button>
            </div>
            <ReactTable
              data={invocationRowsData}
              columns={invocationGridHeadings}
              showPagination={false}
              pageSize={invocationRowsData.length}
              SubComponent={logRow => {
                const finalIndex = logRow.index;
                const finalRow = log.rows[finalIndex];
                const currentPayload = JSON.stringify(
                  finalRow.event.payload,
                  null,
                  4
                );
                // check if response is type JSON
                let finalResponse = finalRow.response;
                try {
                  finalResponse = JSON.parse(finalRow.response);
                  finalResponse = JSON.stringify(finalResponse, null, 4);
                } catch (e) {
                  console.error(e);
                }
                return (
                  <div style={{ padding: '20px' }}>
                    <Tabs
                      animation={false}
                      defaultActiveKey={1}
                      id="requestResponseTab"
                    >
                      <Tab eventKey={1} title="Request">
                        <div className={styles.add_mar_top}>
                          <div className={styles.subheading_text}>Request</div>
                          <AceEditor
                            mode="json"
                            theme="github"
                            name="payload"
                            value={currentPayload}
                            minLines={4}
                            maxLines={100}
                            width="100%"
                            showPrintMargin={false}
                            showGutter={false}
                          />
                        </div>
                      </Tab>
                      <Tab eventKey={2} title="Response">
                        <div className={styles.add_mar_top}>
                          <div className={styles.subheading_text}>Response</div>
                          <AceEditor
                            mode="json"
                            theme="github"
                            name="response"
                            value={finalResponse}
                            minLines={4}
                            maxLines={100}
                            width="100%"
                            showPrintMargin={false}
                            showGutter={false}
                          />
                        </div>
                      </Tab>
                    </Tabs>
                  </div>
                );
              }}
            />
            <div className={styles.loadOlder}>
              <button
                onClick={this.handleOlderEvents.bind(this)}
                className={'btn btn-default'}
              >
                Load older events
              </button>
            </div>
          </div>
        ) : (
          <div className={styles.add_mar_top}>No data available</div>
        )}
        <br />
        <br />
      </div>
    );
  }
}

StreamingLogs.propTypes = {
  log: PropTypes.object,
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
