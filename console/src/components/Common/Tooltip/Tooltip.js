import React from 'react';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';

import { Icon } from '../../UIKit/atoms';
import styles from './Tooltip.scss';

const tooltipGen = message => {
  return <Tooltip id={message}>{message}</Tooltip>;
};

const ToolTip = ({ message }) => (
  <OverlayTrigger placement="right" overlay={tooltipGen(message)}>
    <Icon type="questionCircle" className={styles.tooltipIcon} />
  </OverlayTrigger>
);

export default ToolTip;
