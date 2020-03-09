import React from 'react';
import PropTypes from 'prop-types';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';

import { TooltipStyles } from './Tooltip.style';

// ToolTip Message Generator *********** //

const tooltipGenerator = message => {
  return <Tooltip id={message}>{message}</Tooltip>;
};

// ******************************* //

const ToolTip = props => {
  const { message, placement, children } = props;

  return (
    <OverlayTrigger placement={placement} overlay={tooltipGenerator(message)}>
      <TooltipStyles aria-hidden="true" {...props}>
        {children}
      </TooltipStyles>
    </OverlayTrigger>
  );
};

// PropTypes for Tooltip *********** //

ToolTip.propTypes = {
  message: PropTypes.string,
  children: PropTypes.node,
  placement: PropTypes.oneOf(['top', 'right', 'bottom', 'left']),
};

// Default props for ToolTip ******* //

ToolTip.defaultProps = {
  placement: 'right',
};

// ******************************* //

export default ToolTip;
