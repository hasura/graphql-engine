import React from 'react';
import PropTypes from 'prop-types';
import { Dialog } from '../../../../../new-components/Dialog';
import AceEditor from 'react-ace';
import { connect } from 'react-redux';
//TODO: check
import {
  invokeManualTrigger,
  loadEventInvocations,
  RESET,
} from './InvokeManualTriggerAction';
import { FaSpinner } from 'react-icons/fa';

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

class InvokeManualTrigger extends React.PureComponent {
  constructor(props) {
    super(props);
    this.state = {
      isModalOpen: true,
      pollId: null,
    };
  }

  onModalClose = () => {
    this.setState({
      isModalOpen: false,
    });
    if (this.props.onClose) {
      this.props.onClose();
    }
  };

  componentDidMount() {
    const { name, args, dispatch } = this.props;
    dispatch(invokeManualTrigger(name, args, this.props.source))
      .then(data => {
        const currThis = this;
        const pollId = setInterval(() => {
          dispatch(
            loadEventInvocations(data.event_id, currThis.props.source)
          ).then(d => {
            if (d.length > 0) {
              clearInterval(currThis.state.pollId);
            }
          });
        }, 5000);
        this.setPoll(pollId);
      })
      .catch(err => console.error(`Error invoking trigger: ${err}`));
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
    const { isCreatingManualTrigger, success, status, err, identifier } =
      invokeEventTrigger;
    const loader = () => <FaSpinner className="animate-spin" />;
    const getEventId = () =>
      (isCreatingManualTrigger && loader()) || success.event_id;
    const getEventPayload = () => {
      if (status.length === 0) {
        return <div>Fetching invocation info {loader()}</div>;
      }
      let request;
      let response;
      try {
        request = JSON.parse(status[0].request);
        response = JSON.parse(status[0].response);
      } catch {
        request = "Can't parse request";
        response = "Can't parse response";
      }
      return (
        <div className="flex">
          <div className={`pl-0 w-1/2 mr-sm`}>
            <div> Request </div>
            <AceEditor
              mode="json"
              theme="github"
              name="event_payload"
              value={JSON.stringify(request, null, 2)}
              minLines={8}
              maxLines={15}
              width="100%"
              showPrintMargin={false}
              showGutter={false}
              style={{ backgroundColor: '#fdf9ed', marginTop: '10px' }}
              setOptions={{ useWorker: false }}
            />
          </div>
          <div className={`pl-0 w-1/2`}>
            <div> Latest Invocation Response</div>
            <AceEditor
              mode="json"
              theme="github"
              name="event_payload"
              value={JSON.stringify(response, null, 2)}
              minLines={8}
              maxLines={15}
              width="100%"
              showPrintMargin={false}
              showGutter={false}
              style={{
                backgroundColor: '#fdf9ed',
                marginTop: '10px',
              }}
              setOptions={{ useWorker: false }}
            />
          </div>
        </div>
      );
    };
    /*
     * If there is no error, get the event info. Otherwise check if the err is an object with `error` as key. Print a default message finally
     * */
    const getEventIdErrorText = error => {
      return <span className="text-red-600">{error}</span>;
    };
    const eventInfo =
      (!err && getEventId()) ||
      (err && 'error' in err && getEventIdErrorText(err.error)) ||
      getEventIdErrorText('Unable to invoke trigger');
    const getEventData = () => (
      <div className="pl-0 w-full clear-both pr-0">
        <div className="pl-0 w-full pr-0">
          <div className={`mb-md`}>Event ID - {eventInfo}</div>
          {!err && getEventPayload()}
        </div>
      </div>
    );

    if (!isModalOpen) return null;

    return (
      <div key={identifier}>
        <Dialog
          onClose={this.onModalClose}
          id="invokeEventTrigger"
          title={`Invoking ${name}`}
          className="min-h-[100px]"
          size="xxxl"
          hasBackdrop
        >
          <div className="p-md ">{getEventData()}</div>
        </Dialog>
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
