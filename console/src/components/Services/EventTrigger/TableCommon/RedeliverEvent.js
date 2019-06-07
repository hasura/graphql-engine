import PropTypes from 'prop-types';
import React, { Component } from 'react';
import ReactTable from 'react-table';
import { connect } from 'react-redux';
import Tabs from 'react-bootstrap/lib/Tabs';
import Tab from 'react-bootstrap/lib/Tab';
import Modal from 'react-bootstrap/lib/Modal';
import {
  MODAL_OPEN,
  loadEventInvocations,
  redeliverEvent,
} from '../EventActions';
import AceEditor from 'react-ace';
import 'brace/mode/json';
import Button from '../../../Common/Button/Button';

import { verifySuccessStatus } from '../utils';

class RedeliverEvent extends Component {
  constructor(props) {
    super(props);
    this.state = { isWatching: true, intervalId: null };
    this.refreshData = this.refreshData.bind(this);
  }
  componentDidMount() {
    if (this.props.log.isModalOpen) {
      this.attachFetching(this.props.log.redeliverEventId);
    }
  }
  componentWillReceiveProps(nextProps) {
    if (this.props.log.isModalOpen !== nextProps.log.isModalOpen) {
      if (nextProps.log.isModalOpen === true) {
        this.attachFetching(nextProps.log.redeliverEventId);
      } else {
        this.removeFetching(this.state.intervalId);
      }
    } else if (
      this.state.intervalId !== null &&
      this.props.log.eventInvocations.length ===
        nextProps.log.eventInvocations.length
    ) {
      this.removeFetching(this.state.intervalId);
    }
  }
  componentWillUnmount() {
    if (this.props.log.isModalOpen) {
      this.removeFetching(this.state.intervalId);
    }
  }
  onModalClose = () => {
    this.props.dispatch({ type: MODAL_OPEN, data: false });
  };
  attachFetching(eventId) {
    const intervalId = setInterval(
      () => this.props.dispatch(loadEventInvocations(eventId)),
      5000
    );
    this.setState({ intervalId: intervalId });
  }
  removeFetching(intervalId) {
    clearInterval(intervalId);
    this.setState({ intervalId: null });
  }
  refreshData() {
    this.props.dispatch(loadEventInvocations(this.props.log.event_id));
  }
  handleRedeliver() {
    this.props.dispatch(redeliverEvent(this.props.log.event_id));
  }

  render() {
    const styles = require('./EventTable.scss');
    const { log } = this.props;

    const isLoading = this.state.intervalId ? (
      <i className="fa fa-spinner fa-spin" />
    ) : null;

    const renderTableBody = () => {
      if (log.eventInvocations.length === 0) {
        return <div> No rows found. </div>;
      }
      const invocationColumns = ['status', 'id', 'created_at'];
      const invocationGridHeadings = [];
      invocationColumns.map(column => {
        invocationGridHeadings.push({
          Header: column,
          accessor: column,
        });
      });
      const invocationRowsData = [];
      log.eventInvocations.map(r => {
        const newRow = {};
        const status = verifySuccessStatus(r.status) ? (
          <i
            className={
              styles.invocationSuccess +
              ' fa fa-check invocationsSuccess ' +
              styles.tabletdCenter
            }
          />
        ) : (
          <i
            className={
              styles.invocationFailure + ' fa fa-times invocationsFailure'
            }
          />
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
              return status;
            }
            if (col === 'created_at') {
              const formattedDate = new Date(r.created_at).toLocaleString();
              return formattedDate;
            }
            const content = r[col] === undefined ? 'NULL' : r[col].toString();
            return <div className={conditionalClassname}>{content}</div>;
          };
          newRow[col] = getCellContent();
        });
        invocationRowsData.push(newRow);
      });
      return (
        <ReactTable
          className="invocationClass"
          data={invocationRowsData}
          columns={invocationGridHeadings}
          minRows={0}
          showPagination={false}
          pageSize={invocationRowsData.length}
          SubComponent={logRow => {
            const finalIndex = logRow.index;
            const finalRow = log.eventInvocations[finalIndex];
            // check if response is type JSON
            let finalResponse = finalRow.response;
            try {
              finalResponse = JSON.parse(finalRow.response);
              finalResponse = JSON.stringify(finalResponse, null, 4);
            } catch (e) {
              console.error(e);
              if (typeof finalResponse === 'object') {
                finalResponse = JSON.stringify(finalResponse, null, 4);
              }
            }
            return (
              <div style={{ padding: '20px' }}>
                <Tabs
                  animation={false}
                  defaultActiveKey={1}
                  id="requestResponseTab"
                >
                  <Tab eventKey={1} title="Response">
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
      );
    };

    let latestResponse = '';
    try {
      latestResponse = JSON.stringify(
        JSON.parse(log.eventInvocations[0].response),
        null,
        4
      );
    } catch (e) {
      latestResponse = log.eventInvocations[0]
        ? log.eventInvocations[0].response
        : '';
      if (typeof latestResponse === 'object') {
        latestResponse = JSON.stringify(latestResponse, null, 4);
      }
    }

    return (
      <div>
        <Modal
          show={log.isModalOpen}
          onHide={this.onModalClose}
          dialogClassName={styles.redeliverModal}
          id="redeliverModal"
        >
          <Modal.Header closeButton>
            <Modal.Title>Redeliver Event</Modal.Title>
          </Modal.Header>
          <Modal.Body>
            <div className="content-fluid">
              <div>
                <div
                  className={
                    styles.padd_left_remove +
                    ' col-md-12 ' +
                    styles.padd_right_remove
                  }
                >
                  <div className={styles.add_mar_bottom}>
                    Event ID - {log.redeliverEventId}
                    <Button
                      onClick={this.handleRedeliver.bind(this)}
                      className="hide"
                      color="white"
                      size="sm"
                    >
                      Deliver again
                    </Button>
                  </div>
                  <div className={styles.padd_left_remove + ' col-md-6'}>
                    <div> Request </div>
                    <AceEditor
                      mode="json"
                      theme="github"
                      name="event_payload"
                      value={
                        log.eventInvocations[0]
                          ? JSON.stringify(
                            log.eventInvocations[0].request,
                            null,
                            4
                          )
                          : ''
                      }
                      minLines={10}
                      maxLines={10}
                      width="100%"
                      showPrintMargin={false}
                      showGutter={false}
                      style={{ backgroundColor: '#fdf9ed', marginTop: '10px' }}
                    />
                  </div>
                  <div className={styles.padd_right_remove + ' col-md-6'}>
                    <div> Latest Invocation Response {isLoading}</div>
                    {log.redeliverEventFailure === null ? (
                      <AceEditor
                        mode="json"
                        theme="github"
                        name="event_payload"
                        value={latestResponse}
                        minLines={10}
                        maxLines={10}
                        width="100%"
                        showPrintMargin={false}
                        showGutter={false}
                        style={{
                          backgroundColor: '#fdf9ed',
                          marginTop: '10px',
                        }}
                      />
                    ) : (
                      <AceEditor
                        mode="json"
                        theme="github"
                        name="event_payload"
                        value={JSON.stringify(
                          log.redeliverEventFailure,
                          null,
                          4
                        )}
                        minLines={8}
                        maxLines={10}
                        width="100%"
                        showPrintMargin={false}
                        showGutter={false}
                        style={{
                          backgroundColor: '#fdf9ed',
                          marginTop: '10px',
                        }}
                      />
                    )}
                  </div>
                </div>
                <div
                  className={
                    styles.redeliverEventSection +
                    ' ' +
                    styles.padd_top +
                    ' redeliverEventSection'
                  }
                >
                  <div className={styles.add_mar_bottom}>
                    <em>Recent Invocations</em>
                  </div>
                  {renderTableBody()}
                </div>
              </div>
            </div>
          </Modal.Body>
        </Modal>
      </div>
    );
  }
}

RedeliverEvent.propTypes = {
  log: PropTypes.object.isRequired,
  dispatch: PropTypes.func.isRequired,
};

const mapStateToProps = state => {
  return {
    ...state.triggers,
  };
};

export default connect(mapStateToProps)(RedeliverEvent);
