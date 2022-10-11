import React, { useState } from 'react';
import LoadInspector from './LoadInspector';
import { Tooltip } from '@hasura/console-oss';
import LoadingSpinner from '../Common/LoadingSpinner';

const styles = require('../Metrics.scss');

const inspectRow = require('../images/usage.svg');

const defaultState = {
  isInspecting: false,
};
const Inspect = ({ groupBys, data, RenderLink }) => {
  const [inspectState, toggle] = useState(defaultState);
  const { isInspecting } = inspectState;
  // const { requestId } = props;
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
          RenderLink={RenderLink}
          onHide={onClose}
          data={data}
          groupBys={groupBys}
        />
      );
    }
    return null;
  };
  return (
    <div className={styles.textCenter}>
      {renderIcon()}
      {renderModal()}
    </div>
  );
};

export default Inspect;
