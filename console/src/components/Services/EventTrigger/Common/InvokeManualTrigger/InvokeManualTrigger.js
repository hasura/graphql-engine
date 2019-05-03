import React from 'react';
import PropTypes from 'prop-types';
import Modal from 'react-bootstrap/lib/Modal';
import AceEditor from 'react-ace';
import { connect } from 'react-redux';

import {
  invokeManualTrigger,
  loadEventInvocations,
  RESET,
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
    const { onClose } = this.props;
    if (onClose && typeof onClose !== 'undefined') {
      onClose();
    }
  };
  constructor() {
    super();
    this.state = {
      isModalOpen: true,
      pollId: null,
    };
  }
  componentDidMount() {
    const { name, args, dispatch } = this.props;
    dispatch(invokeManualTrigger(name, args))
      .then(data => {
        const currThis = this;
        const pollId = setInterval(() => {
          dispatch(loadEventInvocations(data.event_id)).then(d => {
            if (d.length > 0) {
              clearInterval(currThis.state.pollId);
            }
          });
        }, 5000);
        this.setPoll(pollId);
      })
      .catch(err => {
        console.error('Error invoking trigger' + err);
      });
  }
  setPoll(pollId) {
    this.setState({
      pollId,
    });
  }
  componentWillUnmount() {
    const { dispatch } = this.props;
    clearInterval(this.state.pollId);
    dispatch({
      type: RESET,
    });
  }
  render() {
    const { isModalOpen } = this.state;
    const { name, invokeEventTrigger } = this.props;
    const {
      isCreatingManualTrigger,
      success,
      status,
      err,
      identifier,
    } = invokeEventTrigger;
    const styles = require('./InvokeManualTrigger.scss');
    const loader = () => <i className="fa fa-spinner fa-spin" />;
    const getEventId = () =>
      (isCreatingManualTrigger && loader()) || success.event_id;
    const getEventPayload = () => {
      if (status.length === 0) {
        return <div>Fetching invocation info {loader()}</div>;
      }
      return (
        <div className={styles.displayFlexContainer}>
          <div className={`${styles.padd_left_remove} col-md-6`}>
            <div> Request </div>
            <AceEditor
              mode="json"
              theme="github"
              name="event_payload"
              value={JSON.stringify(status[0].request, null, 2)}
              minLines={8}
              maxLines={15}
              width="100%"
              showPrintMargin={false}
              showGutter={false}
              style={{ backgroundColor: '#fdf9ed', marginTop: '10px' }}
            />
          </div>
          <div className={`${styles.padd_right_remove} col-md-6`}>
            <div> Latest Invocation Response</div>
            <AceEditor
              mode="json"
              theme="github"
              name="event_payload"
              value={JSON.stringify(status[0].response, null, 2)}
              minLines={8}
              maxLines={15}
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
    /*
     * If there is no error, get the event info. Otherwise check if the err is an object with `error` as key. Print a default message finally
     * */
    const getEventIdErrorText = error => {
      return <span className={styles.error_text}>{error}</span>;
    };
    const eventInfo =
      (!err && getEventId()) ||
      (err && 'error' in err && getEventIdErrorText(err.error)) ||
      getEventIdErrorText('Unable to invoke trigger');
    const getEventData = () => (
      <div
        className={`${styles.remove_padding_left} container-fluid ${
          styles.clear_fix
        } ${styles.padd_right_remove}`}
      >
        <div
          className={`${styles.remove_padding_left} col-md-12 ${
            styles.padd_right_remove
          }`}
        >
          <div className={`${styles.margin_bottom_20} `}>
            Event ID - {eventInfo}
          </div>
          {!err && getEventPayload()}
        </div>
      </div>
    );
    const modalStyle = () => {
      return {
        minHeight: '100px',
      };
    };
    return (
      <div key={identifier}>
        <Modal
          show={isModalOpen}
          style={modalStyle()}
          onHide={this.onModalClose}
          dialogClassName={styles.invoke_trigger_modal}
          id="invokeEventTrigger"
        >
          <Modal.Header closeButton>
            <Modal.Title>Invoking {name}</Modal.Title>
          </Modal.Header>
          <Modal.Body>{getEventData()}</Modal.Body>
        </Modal>
      </div>
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

const mapStateToProps = state => {
  return {
    invokeEventTrigger: state.invokeEventTrigger,
  };
};

export default connect(mapStateToProps)(InvokeManualTrigger);
