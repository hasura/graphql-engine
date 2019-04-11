import React from 'react';
import PropTypes from 'prop-types';
import Modal from 'react-bootstrap/lib/Modal';
import AceEditor from 'react-ace';

import {
  invokeManualTrigger,
  loadEventInvocations,
} from './InvokeManualTriggerAction';

/* This component accepts for following props
 *  1) Trigger name
 *  2) Trigger object
 * It creates a manual trigger and watches the response of the same.
 * */

/*
 * Invokes the event based on the arg object and name and shows the action to the user
 *  - While the request is being made - Invoking
 *  - Once the request is completed
 *    - Success - Track the status
 *    - Failure - Notify user
 * */

class InvokeManualTrigger extends React.Component {
  onModalClose = () => {
    this.setState({
      isModalOpen: false,
    });
  };
  constructor() {
    super();
    this.state = {
      isModalOpen: true,
    };
  }
  componentDidMount() {
    const { name, args, dispatch } = this.props;
    dispatch(invokeManualTrigger(name, args))
      .then(data => {
        const pollId = setInterval(() => {
          dispatch(loadEventInvocations(data.event_id)).then(d => {
            if (d.length > 0) {
              clearInterval(pollId);
            }
          });
        }, 1000);
      })
      .catch(err => {
        console.error('Error invoking trigger' + err);
      });
  }
  render() {
    const { isModalOpen } = this.state;
    const { invokeEventTrigger } = this.props;
    const {
      isCreatingManualTrigger,
      success,
      status,
      // err,
    } = invokeEventTrigger;
    const styles = require('./InvokeManualTrigger.scss');
    const loader = () => <i className="fa fa-spinner fa-spin" />;
    const getEventId = () =>
      (isCreatingManualTrigger && loader()) || success.event_id;
    const getEventPayload = () => {
      if (isCreatingManualTrigger || status.length === 0) {
        return null;
      }
      if (status.length > 0 && !status[0].event.delivered) {
        console.log('Not Delivered');
        console.log(status);
      }
      return (
        <div className={styles.displayFlexContainer}>
          <div className={`${styles.padd_left_remove} col-md-5`}>
            <div> Request </div>
            <AceEditor
              mode="json"
              theme="github"
              name="event_payload"
              value={JSON.stringify(status[0].request, null, 2)}
              minLines={8}
              maxLines={10}
              width="100%"
              showPrintMargin={false}
              showGutter={false}
              style={{ backgroundColor: '#fdf9ed', marginTop: '10px' }}
            />
          </div>
          <div className={`${styles.padd_left_remove} col-md-5`}>
            <div> Latest Invocation Response</div>
            <AceEditor
              mode="json"
              theme="github"
              name="event_payload"
              value={JSON.stringify(status[0].response, null, 2)}
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
          </div>
        </div>
      );
    };
    const getRecentInvocation = () =>
      !isCreatingManualTrigger && (
        <div
          className={`${styles.remove_padding_left} container-fluid ${
            styles.clear_fix
          } ${styles.padding_top_20}`}
        >
          <div className={styles.margin_bottom_20}>
            <em>Recent Invocations</em>
          </div>
          {''}
        </div>
      );
    const getEventData = () => (
      <div className="content-fluid">
        <div className={`${styles.remove_padding_left} col-md-12`}>
          <div className={`${styles.margin_bottom_20} `}>
            Event ID - {getEventId()}
          </div>
          {getEventPayload()}
        </div>
        {getRecentInvocation()}
      </div>
    );
    const modalStyle = () => {
      return {
        minHeight: '100px',
      };
    };
    return (
      <Modal
        show={isModalOpen}
        style={modalStyle()}
        onHide={this.onModalClose}
        dialogClassName={styles.invoke_trigger_modal}
        id="invokeEventTrigger"
      >
        <Modal.Header closeButton>
          <Modal.Title>Trigger Manual Event</Modal.Title>
        </Modal.Header>
        <Modal.Body>{getEventData()}</Modal.Body>
      </Modal>
    );
  }
}

InvokeManualTrigger.propTypes = {
  args: PropTypes.object.isRequired,
  name: PropTypes.string.isRequired,
  onClose: PropTypes.func.isRequired,
  dispatch: PropTypes.func.isRequired,
  invokeEventTrigger: PropTypes.object.isRequired,
};

export default InvokeManualTrigger;
