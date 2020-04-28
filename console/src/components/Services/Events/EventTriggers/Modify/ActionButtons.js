import React from 'react';
import { deleteTrigger } from '../EventActions';
import Button from '../../../../Common/Button/Button';
import { getConfirmation } from '../../../../Common/utils/jsUtils';

const verifyDeleteTrigger = (triggerName, dispatch) => {
  const confirmMessage = `This will permanently delete the event trigger "${triggerName}"`;
  const isOk = getConfirmation(confirmMessage, true, triggerName);
  if (isOk) {
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
