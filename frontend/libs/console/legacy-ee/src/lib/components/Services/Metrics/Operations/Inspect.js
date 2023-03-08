import React, { useState } from 'react';
import LoadInspector from './LoadInspector';
import { Tooltip } from '@hasura/console-legacy-ce';

import LoadingSpinner from '../Common/LoadingSpinner';

import styles from '../Metrics.module.scss';

import inspectRow from '../images/usage.svg';

const defaultState = {
  isInspecting: false,
};
const Inspect = props => {
  const [inspectState, toggle] = useState(defaultState);
  const { isInspecting } = inspectState;
  const { requestId, projectId, time, transport, projectConfigData } = props;
  const inspect = () => {
    toggle({ isInspecting: true });
  };
  const onClose = () => {
    toggle({ isInspecting: false });
  };
  const renderIcon = () => {
    if (isInspecting) {
      return <LoadingSpinner />;
    }
    return (
      <Tooltip side="right" tooltipContentChildren="Inspect">
        <img
          onClick={inspect}
          className={styles.actionImg}
          src={inspectRow}
          alt={'Inspect row'}
        />
      </Tooltip>
    );
  };
  const renderModal = () => {
    if (isInspecting) {
      return (
        <LoadInspector
          onHide={onClose}
          projectId={projectId}
          requestId={requestId}
          time={time}
          transport={transport}
          configData={projectConfigData}
        />
      );
    }
    return null;
  };
  return (
    <div>
      {renderIcon()}
      {renderModal()}
    </div>
  );
};

export default Inspect;
