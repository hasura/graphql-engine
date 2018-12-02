import React from 'react';
import { deleteTrigger } from '../EventActions';

const Buttons = ({ styles, dispatch, triggerName }) => (
  <div className={styles.add_mar_bottom}>
    <button
      className={'btn btn-sm btn-danger'}
      data-test="delete-trigger"
      onClick={() => dispatch(deleteTrigger(triggerName))}
    >
      Delete Trigger
    </button>
  </div>
);

export default Buttons;
