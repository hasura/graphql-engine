import React from 'react';
import Helmet from 'react-helmet';
import { useScheduledTrigger, LocalScheduledTriggerState } from '../state';
import styles from '../ScheduledTriggers.scss';
import Button from '../../../../Common/Button/Button';
import ScheduledTriggerForm from '../../Common/Components/ScheduledTriggerForm';
import { addScheduledTrigger } from '../../ServerIO';
import { ReduxState } from '../../../../../Types';

type AddScheduledTriggerProps = {
  dispatch: any;
  initState?: LocalScheduledTriggerState;
};

const Main = ({ dispatch, initState }: AddScheduledTriggerProps) => {
  const { state, setState } = useScheduledTrigger(initState);

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
        disabled={state.loading}
        className={`${styles.add_mar_right}`}
      >
        {state.loading ? 'Creating...' : 'Create'}
      </Button>
    );
  };

  return (
    <div>
      <Helmet title="Scheduled Triggers | Add" />
      <div className={styles.heading_text}>Add a new scheduled trigger</div>
      <ScheduledTriggerForm state={state} setState={setState} />
      <hr />
      {getSaveButton()}
    </div>
  );
};

const mapStateToProps = (state: ReduxState) => {
  return {
    ...state.events,
  };
};

export default (connect: any) => connect(mapStateToProps)(Main);
