import React from 'react';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import TooltipElement from 'react-bootstrap/lib/Tooltip';
import { FaQuestionCircle } from 'react-icons/fa';

const Tooltip = ({ text, id, className }) => {
  const tooltip = <TooltipElement id={id}>{text}</TooltipElement>;
  return (
    <OverlayTrigger placement="right" overlay={tooltip}>
      <FaQuestionCircle className={className} aria-hidden="true" />
    </OverlayTrigger>
  );
};

export default Tooltip;
