import React from 'react';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import TooltipElement from 'react-bootstrap/lib/Tooltip';

import { Icon } from '../../../../UIKit/atoms';

const Tooltip = ({ text, id, className }) => {
  const tooltip = <TooltipElement id={id}>{text}</TooltipElement>;
  return (
    <OverlayTrigger placement="right" overlay={tooltip}>
      <Icon type="questionCircle" className={className} size={12} />
    </OverlayTrigger>
  );
};

export default Tooltip;
