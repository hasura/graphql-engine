import React from 'react';
import { deleteTrigger } from '../EventActions';
import Button from '../../Layout/Button/Button';

const verifyDeleteTrigger = (triggerName, dispatch) => {
  if (confirm('Are you sure?')) {
    dispatch(deleteTrigger(triggerName));
  }
};

const Buttons = ({ styles, dispatch, triggerName, ongoingRequest }) => (
  <div className={styles.add_mar_bottom}>
    <Button
      color="red"
      size="sm"
      data-test="delete-trigger"
      onClick={() => verifyDeleteTrigger(triggerName, dispatch)}
      disabled={ongoingRequest === 'delete'}
    >
      {ongoingRequest === 'delete' ? 'Deleting ...' : 'Delete'}
    </Button>
  </div>
);

export default Buttons;
