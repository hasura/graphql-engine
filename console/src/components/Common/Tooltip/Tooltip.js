import React from 'react';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';
import styles from './Tooltip.scss';

const tooltipGen = message => {
  return <Tooltip id={message}>{message}</Tooltip>;
};

const ToolTip = ({ message, placement = 'right', children }) => (
  <OverlayTrigger placement={placement} overlay={tooltipGen(message)}>
    {children || (
      <i
        className={`fa fa-question-circle + ${styles.tooltipIcon}`}
        aria-hidden="true"
      />
    )}
  </OverlayTrigger>
);

export default ToolTip;
