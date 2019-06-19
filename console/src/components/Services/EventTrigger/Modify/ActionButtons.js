import React from 'react';
import { deleteTrigger } from '../EventActions';
import Button from '../../../Common/Button/Button';
import { handleDelete } from '../../../../handleDelete';

const verifyDeleteTrigger = (triggerName, dispatch) => {
  const confirmMessage = `'Are you absolutely sure?\nThis action cannot be undone. This will permanently delete the trigger ${triggerName}. Please type "DELETE" (in caps, without quotes) to confirm.\n`;
  const a = handleDelete(confirmMessage);
  if (a && typeof a === 'string' && a.trim() === 'DELETE') {
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
