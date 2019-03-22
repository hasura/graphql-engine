import React, { useState, useEffect } from 'react';
import semverCheck from '../../../../helpers/semver';
import { toggleCascadePendingEvents } from './Actions';

const Options = ({ serverVersion, dispatch, modifyTrigger }) => {
  // set in state what
  const [supportCascade, setSupportCascade] = useState(false);

  const checkServerVersion = () => {
    if (serverVersion) {
      setSupportCascade(semverCheck('triggerDeleteCascade', serverVersion));
    }
  };

  useEffect(checkServerVersion, [serverVersion]);

  const toggle = e => {
    dispatch(toggleCascadePendingEvents(e.target.checked));
  };

  if (supportCascade) {
    return (
      <div style={{ display: 'flex', alignItems: 'center' }}>
        <input
          type="checkbox"
          checked={modifyTrigger.cascadePendingEvents}
          onChange={toggle}
        />
        Cascade
      </div>
    );
  }

  return null;
};

export default Options;
