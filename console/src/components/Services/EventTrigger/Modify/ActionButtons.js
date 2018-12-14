import React from 'react';
import { deleteTrigger } from '../EventActions';

const verifyDeleteTrigger = (triggerName, dispatch) => {
  if (confirm('Are you sure?')) {
    dispatch(deleteTrigger(triggerName));
  }
};

const Buttons = ({ styles, dispatch, triggerName, ongoingRequest }) => (
  <div className={styles.add_mar_bottom}>
    <button
      className={'btn btn-sm btn-danger'}
      data-test="delete-trigger"
      onClick={() => verifyDeleteTrigger(triggerName, dispatch)}
      disabled={ongoingRequest === 'delete'}
    >
      {ongoingRequest === 'delete' ? 'Deleting ...' : 'Delete'}
    </button>
  </div>
);

export default Buttons;
