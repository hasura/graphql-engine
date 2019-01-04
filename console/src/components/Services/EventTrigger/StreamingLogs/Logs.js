import PropTypes from 'prop-types';
import React, { Component } from 'react';
import ReactTable from 'react-table';
import AceEditor from 'react-ace';
import matchSorter from 'match-sorter';
import Tabs from 'react-bootstrap/lib/Tabs';
import Tab from 'react-bootstrap/lib/Tab';
import RedeliverEvent from '../TableCommon/RedeliverEvent';
import TableHeader from '../TableCommon/TableHeader';
import semverCheck from '../../../../helpers/semver';
import parseRowData from './util';
import {
  loadEventLogs,
  setTrigger,
  MODAL_OPEN,
  setRedeliverEvent,
} from '../EventActions';
import {
  vMakeRequest,
  vSetDefaults,
  loadNewerEvents,
  loadOlderEvents,
  toggleLoadingOlder,
  toggleLoadingNewer,
  toggleOldAvailable,
  toggleNewAvailable,
} from './LogActions';
import * as tooltip from '../Common/Tooltips';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';

class StreamingLogs extends Component {
  constructor(props) {
    super(props);
    this.state = {
      isWatching: false,
      intervalId: null,
      filtered: [],
      filterAll: '',
      showRedeliver: false,
    };
    this.refreshData = this.refreshData.bind(this);
    this.filterAll = this.filterAll.bind(this);
    this.props.dispatch(setTrigger(this.props.triggerName));
  }
  componentDidMount() {
    if (this.props.serverVersion) {
      this.checkSemVer(this.props.serverVersion);
    }
    this.props.dispatch(setTrigger(this.props.triggerName));
    this.props.dispatch(loadEventLogs(this.props.triggerName));
  }
  componentWillReceiveProps(nextProps) {
    if (nextProps.serverVersion !== this.props.serverVersion) {
      this.checkSemVer(nextProps.serverVersion);
    }
  }
  componentWillUnmount() {
    this.props.dispatch(vSetDefaults());
  }
  checkSemVer(version) {
    let showRedeliver = false;
    try {
      showRedeliver = semverCheck('eventRedeliver', version);
      if (showRedeliver) {
        this.setState({ ...this.state, showRedeliver: true });
      } else {
        this.setState({ ...this.state, showRedeliver: false });
      }
    } catch (e) {
      console.error(e);
      this.setState({ ...this.state, showRedeliver: false });
    }
  }
  handleNewerEvents() {
    // get the first element
    const firstElement = this.props.log.rows[0];
    const latestTimestamp = firstElement.created_at;
    this.props.dispatch(toggleLoadingNewer(true));
    this.props.dispatch(
      loadNewerEvents(latestTimestamp, this.props.triggerName)
    );
  }
  handleOlderEvents() {
    // get the last element
    const lastElement = this.props.log.rows[this.props.log.rows.length - 1];
    const oldestTimestamp = lastElement.created_at;
    this.props.dispatch(toggleLoadingOlder(true));
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
    this.props.dispatch(toggleOldAvailable(true));
    this.props.dispatch(toggleNewAvailable(true));
    this.props.dispatch(vMakeRequest(this.props.triggerName));
  }
  filterAll(e) {
    const { value } = e.target;
    const filterAll = value;
    const filtered = [{ id: 'all', value: filterAll }];
    // NOTE: this completely clears any COLUMN filters
    this.setState({ filterAll, filtered }, () => {
      const trGroup = document.getElementsByClassName('rt-tr-group');
      const finalTrGroup = trGroup[0];
      finalTrGroup.scrollIntoView({
        behavior: 'smooth',
        block: 'start',
        inline: 'nearest',
        offsetTop: 0,
      });
    });
  }
  toggleModal(currentEvent) {
    // set current event to redeliver
    this.props.dispatch(setRedeliverEvent(currentEvent)).then(() => {
      this.props.dispatch({ type: MODAL_OPEN, data: true });
    });
  }

  render() {
    const {
      triggerName,
      migrationMode,
      log,
      count,
      allSchemas,
      dispatch,
    } = this.props;

    const styles = require('../TableCommon/Table.scss');
    const invocationColumns = [
      'redeliver',
      'status',
      'invocation_id',
      'event_id',
      'operation',
      'primary_key',
      'created_at',
    ];
    const invocationGridHeadings = [];
    invocationColumns.map(column => {
      if (!(column === 'redeliver' && !this.state.showRedeliver)) {
        invocationGridHeadings.push({
          Header: column,
          accessor: column,
        });
      }
    });
    invocationGridHeadings.push({
      // NOTE - this is a "filter all" DUMMY column
      // you can't HIDE it because then it wont FILTER
      // but it has a size of ZERO with no RESIZE and the
      // FILTER component is NULL (it adds a little to the front)
      // You culd possibly move it to the end
      Header: 'All',
      id: 'all',
      width: 0,
      resizable: false,
      sortable: false,
      Filter: () => {},
      getProps: () => {
        return {
          // style: { padding: "0px"}
        };
      },
      filterMethod: (filter, rows) => {
        // using match-sorter
        // it will take the content entered into the "filter"
        // and search for it in EITHER the invocation_id or event_id
        const result = matchSorter(rows, filter.value, {
          keys: [
            'invocation_id.props.children',
            'event_id.props.children',
            'operation.props.children',
          ],
          threshold: matchSorter.rankings.WORD_STARTS_WITH,
        });
        return result;
      },
      filterAll: true,
    });
    const invocationRowsData = [];
    const requestData = [];
    const responseData = [];
    log.rows.map((r, i) => {
      const newRow = {};
      const status =
        r.status === 200 ? (
          <i className={styles.invocationSuccess + ' fa fa-check'} />
        ) : (
          <i className={styles.invocationFailure + ' fa fa-times'} />
        );

      requestData.push(parseRowData(r, 'request'));
      responseData.push(parseRowData(r, 'response'));
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
          if (col === 'operation') {
            return (
              <div className={conditionalClassname}>
                {requestData[i].data.event.op.toLowerCase()}
              </div>
            );
          }
          if (col === 'primary_key') {
            const tableName = requestData[i].data.table.name;
            const tableData = allSchemas.filter(
              row => row.table_name === tableName
            );
            const primaryKey = tableData[0].primary_key.columns; // handle all primary keys
            const pkHtml = [];
            primaryKey.map(pk => {
              const newPrimaryKeyData = requestData[i].data.event.data.new
                ? requestData[i].data.event.data.new[pk]
                : '';
              pkHtml.push(
                <div>
                  {pk} : {newPrimaryKeyData}
                </div>
              );
            });
            return (
              <div className={conditionalClassname}>
                {/* (old) - {oldPrimaryKeyData} | (new) pk - {newPrimaryKeyData} */}
                {pkHtml}
              </div>
            );
          }
          if (col === 'redeliver' && this.state.showRedeliver) {
            return (
              <div className={conditionalClassname}>
                <i
                  onClick={this.toggleModal.bind(this, r.event_id)}
                  className={styles.retryEvent + ' fa fa-repeat'}
                />
              </div>
            );
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
        <div className={'hide'}>
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
              <div className={styles.filterAll}>
                <span>Search:</span>
                <input
                  className={'form-control'}
                  value={this.state.filterAll}
                  onChange={this.filterAll.bind(this)}
                />
              </div>
              <button
                onClick={this.handleNewerEvents.bind(this)}
                className={styles.newBtn + ' btn btn-default'}
              >
                {log.isLoadingNewer ? (
                  <span>
                    Loading... <i className={'fa fa-spinner fa-spin'} />
                  </span>
                ) : (
                  <span>Load newer logs</span>
                )}
              </button>
              {!log.isNewAvailable ? (
                <span> No new logs available at this time </span>
              ) : null}
            </div>
            <ReactTable
              data={invocationRowsData}
              columns={invocationGridHeadings}
              showPagination={false}
              filtered={this.state.filtered}
              pageSize={
                this.state.filterAll !== ''
                  ? this.state.filtered.length
                  : invocationRowsData.length
              }
              SubComponent={logRow => {
                const finalIndex = logRow.index;
                const finalRequest = requestData[finalIndex];
                const finalResponse = responseData[finalIndex];
                return (
                  <div style={{ padding: '20px' }}>
                    <Tabs
                      animation={false}
                      defaultActiveKey={1}
                      id="requestResponseTab"
                    >
                      <Tab eventKey={1} title="Request">
                        {finalRequest.headers ? (
                          <div className={styles.add_mar_top}>
                            <div className={styles.subheading_text}>
                              Headers
                            </div>
                            <AceEditor
                              mode="json"
                              theme="github"
                              name="headers"
                              value={JSON.stringify(
                                finalRequest.headers,
                                null,
                                4
                              )}
                              minLines={4}
                              maxLines={20}
                              width="100%"
                              showPrintMargin={false}
                              showGutter={false}
                            />
                          </div>
                        ) : null}
                        <div className={styles.add_mar_top}>
                          <div className={styles.subheading_text}>Payload</div>
                          <AceEditor
                            mode="json"
                            theme="github"
                            name="payload"
                            value={JSON.stringify(finalRequest.data, null, 4)}
                            minLines={4}
                            maxLines={100}
                            width="100%"
                            showPrintMargin={false}
                            showGutter={false}
                          />
                        </div>
                      </Tab>
                      <Tab eventKey={2} title="Response">
                        {finalResponse.headers ? (
                          <div className={styles.add_mar_top}>
                            <div className={styles.subheading_text}>
                              Headers
                            </div>
                            <AceEditor
                              mode="json"
                              theme="github"
                              name="response"
                              value={JSON.stringify(
                                finalResponse.headers,
                                null,
                                4
                              )}
                              minLines={4}
                              maxLines={20}
                              width="100%"
                              showPrintMargin={false}
                              showGutter={false}
                            />
                          </div>
                        ) : null}
                        <div className={styles.add_mar_top}>
                          <div
                            className={
                              styles.subheading_text +
                              ' col-md-6 ' +
                              styles.padd_remove
                            }
                          >
                            {finalResponse.status_code ? 'Payload' : 'Error'}
                          </div>
                          <div
                            className={
                              styles.status_code_right +
                              ' col-md-6 ' +
                              styles.padd_remove
                            }
                          >
                            {finalResponse.status_code
                              ? [
                                'Status Code: ',
                                finalResponse.status_code === 200 ? (
                                  <i
                                    className={
                                      styles.invocationSuccess +
                                        ' fa fa-check'
                                    }
                                  />
                                ) : (
                                  <i
                                    className={
                                      styles.invocationFailure +
                                        ' fa fa-times'
                                    }
                                  />
                                ),
                                finalResponse.status_code,
                                ' ',
                                <OverlayTrigger
                                  placement="top"
                                  overlay={tooltip.statusCodeDescription}
                                >
                                  <i
                                    className="fa fa-question-circle"
                                    aria-hidden="true"
                                  />
                                </OverlayTrigger>,
                              ]
                              : null}
                          </div>
                          <AceEditor
                            mode="json"
                            theme="github"
                            name="response"
                            value={JSON.stringify(finalResponse.data, null, 4)}
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
              {log.isOldAvailable ? (
                <button
                  onClick={this.handleOlderEvents.bind(this)}
                  className={styles.oldBtn + ' btn btn-default'}
                >
                  {log.isLoadingOlder ? (
                    <span>
                      Loading... <i className={'fa fa-spinner fa-spin'} />
                    </span>
                  ) : (
                    <span>Load older logs</span>
                  )}
                </button>
              ) : (
                <div> No more logs available </div>
              )}
            </div>
          </div>
        ) : (
          <div className={styles.add_mar_top}>No data available</div>
        )}
        <br />
        <br />
        <div className={styles.redeliverModal}>
          <RedeliverEvent log={log} />
        </div>
      </div>
    );
  }
}

StreamingLogs.propTypes = {
  log: PropTypes.object,
  migrationMode: PropTypes.bool.isRequired,
  allSchemas: PropTypes.array.isRequired,
  dispatch: PropTypes.func.isRequired,
};

const mapStateToProps = (state, ownProps) => {
  return {
    ...state.triggers,
    serverVersion: state.main.serverVersion,
    triggerName: ownProps.params.trigger,
    migrationMode: state.main.migrationMode,
    currentSchema: state.tables.currentSchema,
    allSchemas: state.tables.allSchemas,
  };
};

const streamingLogsConnector = connect =>
  connect(mapStateToProps)(StreamingLogs);

export default streamingLogsConnector;
