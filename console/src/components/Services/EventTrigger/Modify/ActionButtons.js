import React, { useState, useEffect } from 'react';
import { deleteTrigger } from '../EventActions';
import Button from '../../../Common/Button/Button';
import semverCheck from '../../../../helpers/semver';

const Buttons = ({
  serverVersion,
  dispatch,
  styles,
  ongoingRequest,
  triggerName,
}) => {
  // semver check to support cascade
  const [supportCascade, setSupportCascade] = useState(false);
  const checkServerVersion = () => {
    if (serverVersion) {
      setSupportCascade(semverCheck('triggerDeleteCascade', serverVersion));
    }
  };
  const [confirmationError, setConfirmationError] = useState(null);
  useEffect(checkServerVersion, [serverVersion]);

  // state for cascade checkbox
  const [shouldCascade, setShouldCascade] = useState(false);

  // delete verification
  const verifyDeleteTrigger = () => {
    let confirmText =
      'Are you sure you want to delete the trigger? The event data will still remain in the database';
    if (shouldCascade && supportCascade) {
      confirmText =
        'This action will delete the trigger and all the associated data. Please type "DELETE" if you want to continue.';
    }
    let confirmation = false;
    if (shouldCascade && supportCascade) {
      const response = window.prompt(confirmText);
      if (response === 'DELETE') {
        confirmation = true;
      } else {
        setConfirmationError(true);
        confirmation = false;
      }
    } else {
      if (window.confirm(confirmText)) {
        confirmation = true;
      } else {
        setConfirmationError(true);
        confirmation = false;
      }
    }
    if (confirmation) {
      dispatch(deleteTrigger(triggerName, shouldCascade, supportCascade));
    }
  };

  // toggle checkbox
  const toggle = e => {
    setShouldCascade(e.target.checked);
  };

  if (supportCascade) {
    return (
      <div className={styles.display_flex}>
        <div className={`${styles.add_mar_right}`}>
          <div>
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
        </div>
        <div className={`${styles.display_flex}`}>
          <div>
            <input
              type="checkbox"
              checked={shouldCascade}
              onChange={toggle}
              className={styles.add_mar_right_small}
            />
          </div>
          <div>Drop pending events</div>
        </div>
        {confirmationError && (
          <div className={`${styles.display_flex}`}>
            <div>
              <span
                className={styles.delete_confirmation_error}
                data-test="delete-confirmation-error"
              >
                * user confirmation error
              </span>
            </div>
          </div>
        )}
      </div>
    );
  }
  return (
    <div className={`${styles.add_mar_right}`}>
      <div>
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
    </div>
  );
};

export default Buttons;
