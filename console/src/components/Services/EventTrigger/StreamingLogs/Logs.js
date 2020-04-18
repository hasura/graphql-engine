import PropTypes from 'prop-types';
import React, { Component } from 'react';
import ReactTable from 'react-table';
import AceEditor from 'react-ace';
import Tabs from 'react-bootstrap/lib/Tabs';
import Tab from 'react-bootstrap/lib/Tab';
// import matchSorter from 'match-sorter';

import RedeliverEvent from '../TableCommon/RedeliverEvent';
import TableHeader from '../TableCommon/TableHeader';
import { parseRowData, verifySuccessStatus } from '../utils';
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
import { convertDateTimeToLocale } from '../utils';
import Button from '../../../Common/Button/Button';
import { NotFoundError } from '../../../Error/PageNotFound';
import { Icon, Spinner, ToolTip, Heading, Box } from '../../../UIKit/atoms';
import styles from '../TableCommon/EventTable.scss';

class StreamingLogs extends Component {
  constructor(props) {
    super(props);
    this.state = {
      isWatching: false,
      intervalId: null,
      filtered: [],
      filterAll: '',
    };
    this.refreshData = this.refreshData.bind(this);
    this.filterAll = this.filterAll.bind(this);
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
      log,
      count,
      dispatch,
      triggerList,
      readOnlyMode,
    } = this.props;

    // check if trigger exists
    const currentTrigger = triggerList.find(s => s.name === triggerName);
    if (!currentTrigger) {
      // throw a 404 exception
      throw new NotFoundError();
    }

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
      invocationGridHeadings.push({
        Header: column,
        accessor: column,
      });
    });

    const invocationRowsData = [];
    const requestData = [];
    const responseData = [];
    log.rows.map((r, i) => {
      const newRow = {};

      const status =
        // 2xx is success
        verifySuccessStatus(r.status) ? (
          <Icon type="check" color="green.original" />
        ) : (
          <Icon type="close" color="red.original" />
        );

      requestData.push(parseRowData(r, 'request'));
      responseData.push(parseRowData(r, 'response'));

      const getCellContent = col => {
        const conditionalClassname = styles.tableCellCenterAlignedOverflow;
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
          const formattedDate = convertDateTimeToLocale(r.created_at);
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
          const primaryKey = currentTrigger.primary_key.columns; // handle all primary keys
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
        if (col === 'redeliver') {
          return (
            <div className={conditionalClassname}>
              <Icon
                type="reload"
                pointer
                onClick={this.toggleModal.bind(this, r.event_id)}
              />
            </div>
          );
        }

        const content = r[col] === undefined ? 'NULL' : r[col].toString();
        return <div className={conditionalClassname}>{content}</div>;
      };

      // Insert cells corresponding to all rows
      invocationColumns.forEach(col => {
        newRow[col] = getCellContent(col);
      });

      invocationRowsData.push(newRow);
    });

    const subComponent = logRow => {
      const finalIndex = logRow.index;
      const finalRequest = requestData[finalIndex];
      const finalResponse = responseData[finalIndex];
      return (
        <div style={{ padding: '20px' }}>
          <Tabs animation={false} defaultActiveKey={1} id="requestResponseTab">
            <Tab eventKey={1} title="Request">
              {finalRequest.headers ? (
                <Box mt="20px">
                  <Heading type="subHeading">Headers</Heading>
                  <AceEditor
                    mode="json"
                    theme="github"
                    name="headers"
                    value={JSON.stringify(finalRequest.headers, null, 4)}
                    minLines={4}
                    maxLines={20}
                    width="100%"
                    showPrintMargin={false}
                    showGutter={false}
                  />
                </Box>
              ) : null}
              <Box mt="20px">
                <Heading type="subHeading">Payload</Heading>
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
              </Box>
            </Tab>
            <Tab eventKey={2} title="Response">
              {finalResponse.headers ? (
                <Box mt="20px">
                  <Heading type="subHeading">Headers</Heading>
                  <AceEditor
                    mode="json"
                    theme="github"
                    name="response"
                    value={JSON.stringify(finalResponse.headers, null, 4)}
                    minLines={4}
                    maxLines={20}
                    width="100%"
                    showPrintMargin={false}
                    showGutter={false}
                  />
                </Box>
              ) : null}
              <Box mt="20px">
                <div className="col-md-6">
                  <Heading type="subHeading" px="0px">
                    {finalResponse.status_code ? 'Payload' : 'Error'}
                  </Heading>
                </div>
                <div
                  className={
                    styles.status_code_right + ' col-md-6 ' + styles.padd_remove
                  }
                >
                  {finalResponse.status_code
                    ? [
                      'Status Code: ',
                      verifySuccessStatus(finalResponse.status_code) ? (
                        <Icon type="check" color="green.original" />
                      ) : (
                        <Icon type="close" color="red.original" />
                      ),
                      finalResponse.status_code,
                      ' ',
                      <ToolTip
                        placement="top"
                        message={tooltip.statusCodeDescription}
                        ml="sm"
                      />,
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
              </Box>
            </Tab>
          </Tabs>
        </div>
      );
    };

    return (
      <div className={styles.container + ' container-fluid'}>
        <TableHeader
          count={count}
          dispatch={dispatch}
          triggerName={triggerName}
          tabName="logs"
          readOnlyMode={readOnlyMode}
        />
        <br />
        <div className={'hide'}>
          <Button
            onClick={this.watchChanges.bind(this)}
            className={styles.watchBtn}
            color="white"
            size="sm"
            data-test="run-query"
          >
            {this.state.isWatching ? (
              <span>
                <Icon type="pause" /> Streaming...
                <Spinner ml="sm" size="12px" display="inline-block" />
              </span>
            ) : (
              <span>
                Stream Logs <Icon type="play" />
              </span>
            )}
          </Button>
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
              <Button
                onClick={this.handleNewerEvents.bind(this)}
                className={styles.add_mar_right}
                color="white"
                size="sm"
              >
                {log.isLoadingNewer ? (
                  <span>
                    Loading...
                    <Spinner ml="sm" size="12px" display="inline-block" />
                  </span>
                ) : (
                  <span>Load newer logs</span>
                )}
              </Button>
              {!log.isNewAvailable ? (
                <span> No new logs available at this time </span>
              ) : null}
            </div>
            <ReactTable
              data={invocationRowsData}
              columns={invocationGridHeadings}
              minRows={0}
              showPagination={false}
              filtered={this.state.filtered}
              pageSize={
                this.state.filterAll !== ''
                  ? this.state.filtered.length
                  : invocationRowsData.length
              }
              SubComponent={subComponent}
            />
            <div className={styles.loadOlder}>
              {log.isOldAvailable ? (
                <Button
                  onClick={this.handleOlderEvents.bind(this)}
                  color="white"
                  size="sm"
                >
                  {log.isLoadingOlder ? (
                    <span>
                      Loading...
                      <Spinner ml="sm" size="sm" display="inline-block" />
                    </span>
                  ) : (
                    <span>Load older logs</span>
                  )}
                </Button>
              ) : (
                <div> No more logs available </div>
              )}
            </div>
          </div>
        ) : (
          <Box mt="20px">No data available</Box>
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
  currentTableSchema: PropTypes.array.isRequired,
  dispatch: PropTypes.func.isRequired,
  triggerList: PropTypes.array.isRequired,
};

const mapStateToProps = (state, ownProps) => {
  return {
    ...state.triggers,
    serverVersion: state.main.serverVersion,
    readOnlyMode: state.main.readOnlyMode,
    triggerName: ownProps.params.trigger,
    currentSchema: state.tables.currentSchema,
    triggerList: state.triggers.triggerList,
  };
};

const streamingLogsConnector = connect =>
  connect(mapStateToProps)(StreamingLogs);

export default streamingLogsConnector;
