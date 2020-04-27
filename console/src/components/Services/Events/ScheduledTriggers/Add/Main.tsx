import React from 'react';
import Helmet from 'react-helmet';
import {
  useScheduledTriggerAdd,
  defaultCronExpr,
  LocalScheduledTriggerState,
} from './state';
import { getEventTargetValue } from '../../../../Common/utils/jsUtils';
import Tooltip from '../../../../Common/Tooltip/Tooltip';
import styles from '../ScheduledTriggers.scss';
import Button from '../../../../Common/Button/Button';
import AceEditor from '../../../../Common/AceEditor/BaseEditor';
import Headers from '../../../../Common/Headers/Headers';
import CronBuilder from '../../../../Common/CronBuilder/CronBuilder';
import { stripNonStandardElements } from '../../../../Common/CronBuilder/utils';
import Modal from '../../../../Common/Modal/Modal';
import { addScheduledTrigger } from '../../ServerIO';
import { EventsState } from '../../state';

type AddScheduledTriggerProps = {
  dispatch: any;
  initState?: LocalScheduledTriggerState;
};

const Main = ({ dispatch, initState }: AddScheduledTriggerProps) => {
  const { state, setState } = useScheduledTriggerAdd(initState);
  const [cronBuilderState, setCronBuilderState] = React.useState(
    defaultCronExpr
  );
  const {
    name,
    webhook,
    schedule,
    payload,
    headers,
    loading,
    showScheduleModal,
  } = state;

  React.useEffect(() => {
    setCronBuilderState(schedule.value || defaultCronExpr);
  }, [schedule.value]);

  const setName = (e: React.BaseSyntheticEvent) =>
    setState.name(getEventTargetValue(e));
  const setWebhookValue = (e: React.BaseSyntheticEvent) =>
    setState.setWebhookValue(getEventTargetValue(e));
  const setTriggerType = (e: React.BaseSyntheticEvent) =>
    setState.scheduleType(getEventTargetValue(e));
  const setScheduleValue = (e: React.BaseSyntheticEvent) =>
    setState.scheduleValue(getEventTargetValue(e));

  const sectionize = (section: JSX.Element) => (
    <div className={styles.add_mar_bottom}>{section}</div>
  );

  const getNameInput = () =>
    sectionize(
      <React.Fragment>
        <h2
          className={`${styles.subheading_text} ${styles.add_mar_bottom_small}`}
        >
          Name
          <Tooltip
            id="trigger-name"
            message={'Name of the trigger'}
            className={styles.add_mar_left_mid}
          />
        </h2>
        <input
          type="text"
          placeholder="name"
          className={`form-control ${styles.inputWidthLarge}`}
          value={name}
          onChange={setName}
        />
      </React.Fragment>
    );

  const getWebhookInput = () =>
    sectionize(
      <React.Fragment>
        <h2
          className={`${styles.subheading_text} ${styles.add_mar_bottom_small}`}
        >
          Webhook
          <Tooltip
            id="trigger-webhook"
            message={'The HTTP endpoint that must be triggered'}
            className={styles.add_mar_left_mid}
          />
        </h2>
        <input
          type="text"
          placeholder="webhook"
          className={`form-control ${styles.inputWidthLarge}`}
          value={webhook.value}
          onChange={setWebhookValue}
        />
      </React.Fragment>
    );

  const getTriggerTypeInput = () => {
    return sectionize(
      <React.Fragment>
        <h2
          className={`${styles.subheading_text} ${styles.add_mar_bottom_small}`}
        >
          Trigger Type
          <Tooltip
            id="trigger-type"
            message={
              'Scheduled triggers could be scheduled with a cron and adhoc triggers can be invoked manually'
            }
            className={styles.add_mar_left_mid}
          />
        </h2>
        <select
          className={`form-control ${styles.inputWidthLarge}`}
          value={schedule.type}
          onChange={setTriggerType}
        >
          <option key="cron" value="cron">
            Cron
          </option>
          <option key="adhoc" value="adhoc">
            Adhoc
          </option>
        </select>
      </React.Fragment>
    );
  };

  const getScheduleInput = () => {
    if (schedule.type === 'adhoc') return null;

    const getScheduleInputText = (disabled: boolean, className: string) => (
      <input
        type="text"
        placeholder={defaultCronExpr}
        className={`form-control ${styles.inputWidthLarge} ${className}`}
        value={schedule.value}
        onChange={setScheduleValue}
        disabled={disabled}
      />
    );

    const modalOnSubmit = () => {
      setState.scheduleValue(stripNonStandardElements(cronBuilderState));
      setState.toggleScheduleModal();
    };
    const modalReset = () => {
      setCronBuilderState(schedule.value || '');
      setState.toggleScheduleModal();
    };
    const modal = (
      <Modal
        show={!!showScheduleModal}
        title="Build your CRON expression"
        onClose={modalReset}
        onCancel={modalReset}
        onSubmit={modalOnSubmit}
        submitText={'Use'}
      >
        <div>
          <CronBuilder
            value={cronBuilderState}
            showResultText
            showResultCron
            onChange={setCronBuilderState}
          />
        </div>
      </Modal>
    );
    return sectionize(
      <React.Fragment>
        <h2
          className={`${styles.subheading_text} ${styles.add_mar_bottom_small}`}
        >
          Cron schedule
          <Tooltip
            id="trigger-schedule"
            message={'Schedule for your cron'}
            className={styles.add_mar_left_mid}
          />
        </h2>
        <div className={`${styles.add_mar_bottom_mid} ${styles.display_flex}`}>
          {getScheduleInputText(false, styles.add_mar_right_mid)}
          {modal}
          <a
            onClick={setState.toggleScheduleModal}
            className={styles.cursorPointer}
          >
            Build a cron expression
          </a>
        </div>
      </React.Fragment>
    );
  };

  // TODO make JSONEditor component
  const getPayloadInput = () =>
    sectionize(
      <React.Fragment>
        <h2
          className={`${styles.subheading_text} ${styles.add_mar_bottom_small}`}
        >
          Payload
          <Tooltip
            id="trigger-payload"
            message={'The request payload for the HTTP trigger'}
            className={styles.add_mar_left_mid}
          />
        </h2>
        <AceEditor
          mode="json"
          value={payload}
          onChange={setState.payload}
          height={'200px'}
        />
      </React.Fragment>
    );

  const getHeadersInput = () =>
    sectionize(
      <React.Fragment>
        <h2
          className={`${styles.subheading_text} ${styles.add_mar_bottom_small}`}
        >
          Headers
          <Tooltip
            id="trigger-headers"
            message={'Configure headers for the request to the webhook'}
            className={styles.add_mar_left_mid}
          />
        </h2>
        <Headers headers={headers} setHeaders={setState.headers} />
      </React.Fragment>
    );

  const getSaveButton = () => {
    const callback = () => setState.loading(false);
    const onClick = (e: React.SyntheticEvent) => {
      e.preventDefault();
      setState.loading(true);
      dispatch(addScheduledTrigger(state, callback, callback));
    };
    return (
      <Button
        onClick={onClick}
        color="yellow"
        size="sm"
        disabled={loading}
        className={`${styles.add_mar_right}`}
      >
        {loading ? 'Creating...' : 'Create'}
      </Button>
    );
  };

  return (
    <div>
      <Helmet title="Scheduled Triggers | Add" />
      <div className={styles.heading_text}>Add a new scheduled trigger</div>
      {getNameInput()}
      {getWebhookInput()}
      {getTriggerTypeInput()}
      {getScheduleInput()}
      {getPayloadInput()}
      {getHeadersInput()}
      <hr />
      {getSaveButton()}
    </div>
  );
};

const mapStateToProps = (state: { events: EventsState }) => {
  return {
    ...state.events,
  };
};

export default (connect: any) => connect(mapStateToProps)(Main);
