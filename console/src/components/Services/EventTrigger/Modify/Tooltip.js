import React from 'react';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';

const tooltipGen = message => {
  return <Tooltip id={message}>{message}</Tooltip>;
};

const ToolTip = ({ message }) => (
  <OverlayTrigger placement="right" overlay={tooltipGen(message)}>
    <i className="fa fa-question-circle" aria-hidden="true" />
  </OverlayTrigger>
);

export default ToolTip;
