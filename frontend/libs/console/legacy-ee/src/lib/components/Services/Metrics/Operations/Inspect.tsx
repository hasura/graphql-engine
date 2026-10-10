import { useState } from 'react';
import LoadInspector from './LoadInspector';

import styles from '../Metrics.module.scss';

import inspectRow from '../images/usage.svg';
import { SkeletonList, Tooltip } from '@hasura/shared/ui';

const defaultState = {
  isInspecting: false,
};
const Inspect = (props) => {
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
      return <SkeletonList count={8} />;
    }

    return (
      <Tooltip side="right" content="Inspect">
        <img
          onClick={inspect}
          className={styles['actionImg']}
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
