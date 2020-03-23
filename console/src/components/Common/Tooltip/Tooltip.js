import React from 'react';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';

import { Icon } from '../../UIKit/atoms';

const tooltipGen = message => {
  return <Tooltip id={message}>{message}</Tooltip>;
};

const ToolTip = ({ message }) => (
  <OverlayTrigger placement="right" overlay={tooltipGen(message)}>
    <Icon type="questionCircle" ml="xs" size={12} />
  </OverlayTrigger>
);

export default ToolTip;
