import React from 'react';
import Helmet from 'react-helmet';
import { connect, ConnectedProps } from 'react-redux';
import { useScheduledTrigger, LocalScheduledTriggerState } from '../state';
import styles from '../ScheduledTriggers.scss';
import Button from '../../../../Common/Button/Button';
import CronTriggerFrom from '../../Common/Components/CronTriggerForm';
import {
  getReactHelmetTitle,
  mapDispatchToPropsEmpty,
} from '../../../../Common/utils/reactUtils';
import { MapStateToProps } from '../../../../../types';
import { addScheduledTrigger } from '../../ServerIO';
import { EVENTS_SERVICE_HEADING, CRON_TRIGGER } from '../../constants';

interface Props extends InjectedProps {
  initState?: LocalScheduledTriggerState;
}

const Main: React.FC<Props> = props => {
  const { dispatch, initState, readOnlyMode } = props;
  const { state, setState } = useScheduledTrigger(initState);

  const callback = () => setState.loading('add', false);
  const onSave = (e: React.SyntheticEvent) => {
    e.preventDefault();
    setState.loading('add', true);
    dispatch(addScheduledTrigger(state, callback, callback));
  };

  return (
    <div className={styles.add_mar_bottom}>
      <Helmet
        title={getReactHelmetTitle(
          `Create ${CRON_TRIGGER}`,
          EVENTS_SERVICE_HEADING
        )}
      />
      <div className={`${styles.heading_text} ${styles.add_mar_bottom}`}>
        Create a cron trigger
      </div>
      <CronTriggerFrom state={state} setState={setState} />
      {!readOnlyMode && (
        <Button
          onClick={onSave}
          color="yellow"
          size="sm"
          disabled={state.loading.add}
          className={`${styles.add_mar_right}`}
        >
          {state.loading.add ? 'Creating...' : 'Create'}
        </Button>
      )}
    </div>
  );
};

type PropsFromState = {
  readOnlyMode: boolean;
};
const mapStateToProps: MapStateToProps<PropsFromState> = state => ({
  readOnlyMode: state.main.readOnlyMode,
});

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);
type InjectedProps = ConnectedProps<typeof connector>;

const AddConnector = connector(Main);
export default AddConnector;
